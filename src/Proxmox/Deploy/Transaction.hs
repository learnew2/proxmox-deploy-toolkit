{- Copyright (C) 2025 Ilya Zamaratskikh

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, see <http://www.gnu.org/licenses>. -}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
module Proxmox.Deploy.Transaction
  ( planTransactionStages
  , planTransactionActions
  , getVMIDRange
  , executeTransaction
  ) where

import           Control.Concurrent
import           Control.Monad                            (unless, when)
import           Control.Monad.Catch
import           Control.Monad.Except
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.State
import           Control.Monad.Trans.Writer
import           Data.Aeson                               (Value (..))
import           Data.Functor                             ((<&>))
import           Data.Functor.Identity
import           Data.List                                (nub, sortOn)
import           Data.Map                                 (Map)
import qualified Data.Map                                 as M
import           Data.Maybe
import           Data.Text                                (Text)
import qualified Data.Text                                as T
import           Proxmox.Agent.Client
import           Proxmox.Client
import           Proxmox.Deploy.Models.Config
import           Proxmox.Deploy.Models.Config.Deploy
import           Proxmox.Deploy.Models.Config.DeployAgent
import           Proxmox.Deploy.Models.Config.Network
import           Proxmox.Deploy.Models.Config.Template
import           Proxmox.Deploy.Models.Config.VM
import           Proxmox.Deploy.Models.Transaction
import qualified Proxmox.Deploy.Models.Transaction        as TS
import           Proxmox.Deploy.Ssl
import           Proxmox.Deploy.Types
import           Proxmox.Models
import           Proxmox.Models.Network
import           Proxmox.Models.SDNNetwork
import           Proxmox.Models.SDNZone
import           Proxmox.Models.Snapshot
import           Proxmox.Models.Storage
import           Proxmox.Models.VM
import qualified Proxmox.Models.VM                        as VM
import           Proxmox.Models.VMClone
import           Proxmox.Models.VMConfig
import           Proxmox.Retry
import           Proxmox.Schema                           (ProxmoxState (ProxmoxState))
import           Servant.Client                           (parseBaseUrl)

snapshotPresent :: Text -> ProxmoxResponse [ProxmoxSnapshot] -> Bool
snapshotPresent snapName (ProxmoxResponse { proxmoxData = snaps }) = any ((==) snapName . snapshotName) snaps

snapshotNotPresent :: Text -> ProxmoxResponse [ProxmoxSnapshot] -> Bool
snapshotNotPresent snapName resp = not $ snapshotPresent snapName resp

vmDeviceNotPresent :: String -> ProxmoxResponse (Maybe ProxmoxVMConfig) -> Bool
vmDeviceNotPresent deviceName resp = not $ vmDevicePresent deviceName resp

vmDevicePresent :: String -> ProxmoxResponse (Maybe ProxmoxVMConfig) -> Bool
vmDevicePresent _ (ProxmoxResponse { proxmoxData = Nothing }) = False
vmDevicePresent deviceName (ProxmoxResponse { proxmoxData = Just cfg }) = ((deviceName `elem`) . M.keys . vmConfigMap) cfg

vmNetworksEmpty :: ProxmoxResponse (Maybe ProxmoxVMConfig) -> Bool
vmNetworksEmpty (ProxmoxResponse { proxmoxData = Nothing }) = False
vmNetworksEmpty (ProxmoxResponse { proxmoxData = Just cfg }) = (null . vmConfigNetworkNumbers) cfg

-- function for waitForClient, checks vm by id and its power
vmStateIs :: ProxmoxResponse (Maybe ProxmoxVMStatusWrapper) -> ProxmoxVMStatus -> Bool
vmStateIs (ProxmoxResponse { proxmoxData = Just (ProxmoxVMStatusWrapper status) }) = (==status)
vmStateIs _ = const False

vmExists :: Int -> M.Map Int ProxmoxVM -> Bool
vmExists vmid vmMap = isJust $ M.lookup vmid vmMap

vmNotExists :: Int -> M.Map Int ProxmoxVM -> Bool
vmNotExists vmid vmMap = not $ vmExists vmid vmMap

vmUnlocked :: ProxmoxResponse (Maybe ProxmoxVMConfig) -> Bool
vmUnlocked (ProxmoxResponse Nothing _) = False
vmUnlocked (ProxmoxResponse { proxmoxData = Just ProxmoxVMConfig { vmLock = vmLock }}) = isNothing vmLock

sdnNetworkExists :: String -> ProxmoxResponse [ProxmoxNetwork] -> Bool
sdnNetworkExists vnetName (ProxmoxResponse { proxmoxData = networks }) = any (\x -> proxmoxNetworkType x == Vnet && proxmoxNetworkInterface x == vnetName) networks

-- looks up for transaction data and deploy config for vmid
getVMID :: String -> TransactionData -> DeployConfig -> Maybe Int
getVMID vmName (TransactionData vmIDMap) (DeployConfig {deployVMs=vms }) = do
  case M.lookup vmName vmIDMap of
    (Just vmid) -> Just vmid
    Nothing -> case filter ((==vmName) . configVMName) vms of
      []     -> Nothing
      (vm:_) -> configVMID vm

executeTransactionAction :: TransactionAction -> StatefulTransactionT ()
executeTransactionAction (AssignVMID vmName) = do
  (TransactionState { .. }) <- get
  (TransactionData vmIDMap) <- transactionDataGetF
  case M.lookup vmName vmIDMap of
    (Just _) -> $(logWarn) $ T.pack $ "VM " <> vmName <> " already has allocated VMID. Skipping."
    Nothing -> do
      vmid <- transactionAllocateVMIDF
      let ntData = TransactionData (M.insert vmName vmid vmIDMap)
      () <- transactionDataSetF ntData
      return ()
executeTransactionAction (UnassignVMID vmName) = do
  (TransactionState { .. }) <- get
  d@(TransactionData vmIDMap) <- transactionDataGetF
  case M.lookup vmName vmIDMap of
    Nothing -> $(logWarn) $ T.pack $ "VM " <> vmName <> " has no allocated VMID. Skipping."
    (Just _) -> do
      let ntData = d { transactionIDMap = M.delete vmName vmIDMap }
      () <- transactionDataSetF ntData
      return ()
executeTransactionAction (DestroyVM vmName) = do
  (TransactionState { transactionDeployConfig = deployConfig@(DeployConfig {deployParameters = (DeployParams { deployNodeName = nodeName }) }),.. }) <- get
  data' <- transactionDataGetF
  case getVMID vmName data' deployConfig of
    Nothing -> $(logWarn) $ T.pack $ "VM " <> vmName <> " has no allocated VMID"
    (Just vmid) -> do
      vmMap <- (defaultRetryClient' transactionProxmoxState $ getActiveNodesVMMap) >>= defaultClientErrorWrapper
      if vmExists vmid vmMap then do
        _ <- (defaultRetryClient' transactionProxmoxState) $ (deleteVM' nodeName vmid defaultProxmoxVMDeleteRequest)
        deleteResult <- waitForClient
          60_000_000
          ("VM " <> (T.pack . show) vmid <> " still exists. Waiting...")
          5
          1_000_000
          (defaultRetryClient' transactionProxmoxState getActiveNodesVMMap)
          (vmNotExists vmid)
        case deleteResult of
          (Left e) -> throwError (ClientError e)
          (Right True) -> $(logInfo) $ T.pack $ "VM " <> show vmName <> " is deleted"
          (Right False) -> throwError (VMDeleteError vmid)
      else $(logWarn) $ T.pack $ "VM " <> show vmName <> " is not found by its VMID (" <> show vmid <> ")"
executeTransactionAction (StopVM vmName) = do
  (TransactionState { transactionDeployConfig = deployConfig@(DeployConfig {deployParameters = (DeployParams { deployNodeName = nodeName }) }),.. }) <- get
  data' <- transactionDataGetF
  case getVMID vmName data' deployConfig of
    Nothing -> $(logWarn) $ T.pack $ "VM " <> vmName <> " has no allocated VMID"
    (Just vmid) -> do
      vmMap' <- (defaultRetryClient' transactionProxmoxState) getActiveNodesVMMap
      case vmMap' of
        (Left e) -> throwError (ClientError e)
        (Right vmMap) -> do
          case M.lookup vmid vmMap of
            Nothing -> $(logWarn) $ T.pack $ "VM with VMID " <> show vmid <> " not found."
            _ -> do
              powerResult <- waitForClient
                60_000_000
                ("VM " <> (T.pack . show) vmid <> " is not powered on. Waiting...")
                15
                1_000_000
                (defaultRetryClient' transactionProxmoxState (stopVM nodeName vmid >> (liftIO . threadDelay) 5_000_000 >> getVMPower nodeName vmid))
                (`vmStateIs` VM.VMStopped)
              case powerResult of
                (Left e) -> throwError (ClientError e)
                (Right True) -> $(logInfo) $ T.pack $ "Turned off VM " <> vmName <> "(#" <> show vmid <> ")"
                (Right False) -> $(logWarn) $ T.pack $ "Failed to stop VM " <> vmName <> "(#" <> show vmid <> ")"
executeTransactionAction (StartVM vmName) = do
  (TransactionState { transactionDeployConfig = deployConfig@(DeployConfig {deployParameters = (DeployParams { deployNodeName = nodeName }) }),.. }) <- get
  data' <- transactionDataGetF
  case getVMID vmName data' deployConfig of
    Nothing -> $(logWarn) $ T.pack $ "VM " <> vmName <> " has no allocated VMID"
    (Just vmid) -> do
      vmMap <- (defaultRetryClient' transactionProxmoxState) getActiveNodesVMMap >>= defaultClientErrorWrapper
      case M.lookup vmid vmMap of
        Nothing -> $(logWarn) $ T.pack $ "VM with VMID " <> show vmid <> " not found."
        _ -> do
          powerResult <- waitForClient
            60_000_000
            ("VM " <> (T.pack . show) vmid <> " is not powered on. Waiting...")
            10
            1_000_000
            (defaultRetryClient' transactionProxmoxState (startVM nodeName vmid >> (liftIO . threadDelay) 5_000_000 >> getVMPower nodeName vmid))
            (`vmStateIs` VM.VMRunning)
          case powerResult of
            (Left e) -> throwError (ClientError e)
            (Right True) -> $(logInfo) $ T.pack $ "Turned on VM " <> vmName <> "(#" <> show vmid <> ")"
            (Right False) -> $(logWarn) $ T.pack $ "Failed to start VM " <> vmName <> "(#" <> show vmid <> ")"
executeTransactionAction (DetachNetwork vmName networkNumber) = do
  (TransactionState { transactionDeployConfig = deployConfig@(DeployConfig {deployParameters = (DeployParams { deployNodeName = nodeName }) }),.. }) <- get
  data' <- transactionDataGetF
  case getVMID vmName data' deployConfig of
    Nothing -> $(logWarn) $ T.pack $ "VM " <> vmName <> " has no allocated VMID"
    (Just vmid) -> do
      (ProxmoxResponse { proxmoxData = vmConfig'}) <- (defaultRetryClient' transactionProxmoxState) (getVMConfig nodeName vmid) >>= defaultClientErrorWrapper
      case vmConfig' of
        Nothing -> $(logWarn) $ T.pack $ "VM with VMID " <> show vmid <> " not found."
        _ -> do
          let networkDevice = "net" <> show networkNumber
          _ <- waitForClient
            60_000_000
            (T.pack $ "Waiting for configuration change of VM " <> show vmid)
            10
            1_000_000
            (defaultRetryClient' transactionProxmoxState $ deleteVMConfig nodeName vmid [networkDevice] >> (liftIO . threadDelay) 2_000_000 >> getVMConfig nodeName vmid)
            (vmDeviceNotPresent networkDevice)
          pure ()
executeTransactionAction (AttachNetwork vmName networkConfig) = do
  (TransactionState { transactionDeployConfig = deployConfig@(DeployConfig {deployParameters = (DeployParams { deployNodeName = nodeName }) }),.. }) <- get
  data' <- transactionDataGetF
  case getVMID vmName data' deployConfig of
    Nothing -> $(logWarn) $ T.pack $ "VM " <> vmName <> " has no allocated VMID"
    (Just vmid) -> do
      (ProxmoxResponse { proxmoxData = vmConfig'}) <- (defaultRetryClient' transactionProxmoxState) (getVMConfig nodeName vmid) >>= defaultClientErrorWrapper
      case vmConfig' of
        Nothing -> $(logWarn) $ T.pack $ "VM with VMID " <> show vmid <> " not found."
        _ -> do
          case formatConfigVMNetwork networkConfig of
            Nothing -> throwError (UnknownError $ "Failed to format network device string: " <> show networkConfig)
            (Just (deviceName, deviceConfig)) -> do
              _ <- (defaultRetryClient' transactionProxmoxState) (putVMConfig nodeName vmid (M.fromList [(deviceName, (String . T.pack) deviceConfig)])) >>= defaultClientErrorWrapper
              _ <- waitForClient
                60_000_000
                (T.pack $ "Waiting for configuration change of VM " <> show vmid)
                10
                1_000_000
                (defaultRetryClient' transactionProxmoxState $ getVMConfig nodeName vmid)
                (vmDevicePresent deviceName)
              pure ()
executeTransactionAction (SetVMDisplay vmName vmDisplay) = do
  (TransactionState { transactionProxmoxState = proxmoxState, transactionDeployConfig = deployConfig@(DeployConfig {deployParameters = (DeployParams { deployNodeName = nodeName }), deployAgent = deployAgent }),.. }) <- get
  case deployAgent of
    Nothing -> $(logWarn) $ T.pack $ "Proxmox FS agent config is not provided."
    (Just (DeployAgentConfig {configAgentURL=url, configAgentToken=token, configAgentDisplayNetwork=displayNet})) -> do
      parsedUrl <- (parseBaseUrl . T.unpack) url
      manager <- liftIO $ createSSLManager deployConfig
      let agentState = ProxmoxState parsedUrl manager
      data' <- transactionDataGetF
      case getVMID vmName data' deployConfig of
        Nothing -> $(logWarn) $ T.pack $ "VM " <> vmName <> " has no allocated VMID"
        (Just vmid) -> do
          (ProxmoxResponse { proxmoxData = vmConfig'}) <- (defaultRetryClient' proxmoxState) (getVMConfig nodeName vmid) >>= defaultClientErrorWrapper
          case vmConfig' of
            Nothing -> $(logWarn) $ T.pack $ "VM with VMID " <> show vmid <> " not found."
            _ -> do
              res <- (defaultRetryClient' agentState) $ setVNCPort vmid (Just $ AgentToken token) (VNCRequest {reqNetwork=displayNet, reqDisplay=vmDisplay})
              case res of
                (Left e) -> throwError (UnknownError $ "Agent change display error: " <> show e)
                (Right _) -> do
                  $(logInfo) $ T.pack $ "Display of VM " <> vmName <> " changed"
                  pure ()
executeTransactionAction (MakeSnapshot vmName snapParams) = do
  (TransactionState { transactionDeployConfig = deployConfig@(DeployConfig {deployParameters = (DeployParams { deployNodeName = nodeName }) }),.. }) <- get
  data' <- transactionDataGetF
  case getVMID vmName data' deployConfig of
    Nothing -> $(logWarn) $ T.pack $ "VM " <> vmName <> " has no allocated VMID"
    (Just vmid) -> do
      vmMap <- (defaultRetryClient' transactionProxmoxState) (getNodeVMsMap nodeName) >>= defaultClientErrorWrapper
      case M.lookup vmid vmMap of
        Nothing -> $(logWarn) $ T.pack $ "VM with VMID " <> show vmid <> " not found."
        _ -> do
          _ <- (defaultRetryClientC' transactionProxmoxState) (createSnapshot nodeName vmid snapParams) >>= defaultClientErrorWrapper
          _ <- waitForClient
            300_000_000
            (T.pack $ "Waiting for snapshotting of VM " <> vmName)
            60
            1_000_000
            (defaultRetryClient' transactionProxmoxState $ getVMSnapshots nodeName vmid)
            (snapshotPresent (snapshotCreateName snapParams))
          pure ()
executeTransactionAction (DeleteSnapshot vmName (ProxmoxSnapshotCreate { snapshotCreateName = snapName })) = do
  (TransactionState { transactionDeployConfig = deployConfig@(DeployConfig {deployParameters = (DeployParams { deployNodeName = nodeName }) }),.. }) <- get
  data' <- transactionDataGetF
  case getVMID vmName data' deployConfig of
    Nothing -> $(logWarn) $ T.pack $ "VM " <> vmName <> " has no allocated VMID"
    (Just vmid) -> do
      vmMap <- (defaultRetryClient' transactionProxmoxState) (getNodeVMsMap nodeName) >>= defaultClientErrorWrapper
      case M.lookup vmid vmMap of
        Nothing -> $(logWarn) $ T.pack $ "VM with VMID " <> show vmid <> " not found."
        _ -> do
          _ <- (defaultRetryClient' transactionProxmoxState) (deleteVMSnapshot nodeName vmid snapName) >>= defaultClientErrorWrapper
          _ <- waitForClient
            300_000_000
            (T.pack $ "Waiting for snapshotting of VM " <> vmName)
            60
            1_000_000
            (defaultRetryClient' transactionProxmoxState $ getVMSnapshots nodeName vmid)
            (snapshotNotPresent snapName)
          pure ()
executeTransactionAction (RollbackVM vmName snapName) = do
  (TransactionState { transactionDeployConfig = deployConfig@(DeployConfig {deployParameters = (DeployParams { deployNodeName = nodeName }) }),.. }) <- get
  data' <- transactionDataGetF
  case getVMID vmName data' deployConfig of
    Nothing -> $(logWarn) $ T.pack $ "VM " <> vmName <> " has no allocated VMID"
    (Just vmid) -> do
      vmMap <- (defaultRetryClient' transactionProxmoxState) (getNodeVMsMap nodeName) >>= defaultClientErrorWrapper
      case M.lookup vmid vmMap of
        Nothing -> $(logWarn) $ T.pack $ "VM with VMID " <> show vmid <> " not found."
        _ -> do
          _ <- (defaultRetryClient' transactionProxmoxState) (rollbackVM nodeName vmid (T.pack snapName) (ProxmoxRollbackParams True)) >>= defaultClientErrorWrapper
          _ <- waitForClient
            300_000_000
            (T.pack $ "Waiting for restoring of VM " <> vmName)
            60
            1_000_000
            (defaultRetryClient' transactionProxmoxState $ getVMPower nodeName vmid)
            (`vmStateIs` VM.VMStopped)
          pure ()
executeTransactionAction (RemoveNetworks vmName) = do
  (TransactionState { transactionDeployConfig = deployConfig@(DeployConfig {deployParameters = (DeployParams { deployNodeName = nodeName }) }),.. }) <- get
  data' <- transactionDataGetF
  case getVMID vmName data' deployConfig of
    Nothing -> $(logWarn) $ T.pack $ "VM " <> vmName <> " has no allocated VMID"
    (Just vmid) -> do
      vmMap <- (defaultRetryClient' transactionProxmoxState) getActiveNodesVMMap >>= defaultClientErrorWrapper
      case M.lookup vmid vmMap of
        Nothing -> $(logWarn) $ T.pack $ "VM with VMID " <> show vmid <> " not found."
        _ -> do
          (ProxmoxResponse { proxmoxData = vmCfg'}) <- (defaultRetryClient' transactionProxmoxState) (getVMConfig nodeName vmid) >>= defaultClientErrorWrapper
          case vmCfg' of
            Nothing -> throwError (VMConfigIsNotFound vmName)
            (Just vmCfg) -> do
              _ <- (defaultRetryClient' transactionProxmoxState) (deleteVMConfig nodeName vmid (map (\x -> "net" <> show x) (vmConfigNetworkNumbers vmCfg)))
              _ <- waitForClient
                60_000_000
                (T.pack $ "Waiting for configuration change of VM " <> show vmid)
                10
                1_000_000
                (defaultRetryClient' transactionProxmoxState $ getVMConfig nodeName vmid)
                vmNetworksEmpty
              pure ()
executeTransactionAction (CloneVM params@(ProxmoxVMCloneParams { proxmoxVMCloneName = vmName',.. })) = do
  let vmName = T.unpack $ fromJust vmName'
  (TransactionState { transactionDeployConfig = deployConfig@(DeployConfig {deployTemplates = templates}),.. }) <- get
  data' <- transactionDataGetF
  -- getting some overengineering with passing VMID from clone params
  -- but it is used in cloning, so idk at least now
  case getVMID vmName data' deployConfig of
    Nothing -> throwError (MachineHasNoID vmName)
    (Just vmid) -> do
      nodeMap <- (defaultRetryClient' transactionProxmoxState) getActiveNodeVMNodeMap >>= defaultClientErrorWrapper
      case M.lookup vmid nodeMap of
        (Just _) -> $(logWarn) $ T.pack $ "VM #" <> show vmid <> " already exists!"
        Nothing -> do
          case M.lookup proxmoxVMCloneVMID nodeMap of
            Nothing -> case filter ((==proxmoxVMCloneVMID) . configTemplateID) templates of
              (template:_) -> throwError (TemplateNodeNotFound template)
              []           -> error "unreachable"
            (Just templateNodeName) -> do
              let fullParams = params { proxmoxVMCloneNewID = vmid }
              cloneRes' <- (defaultRetryClient' transactionProxmoxState) $ cloneVM (T.pack templateNodeName) proxmoxVMCloneVMID fullParams
              case cloneRes' of
                (Left e) -> throwError (ClientError e)
                (Right _) -> do
                  $(logInfo) "VM is cloned, awaiting for unlocking VM..."
                  unlockResult <- waitForClient
                    60_000_000
                    ("VM " <> (T.pack . show) vmid <> " is locked. Waiting...")
                    120
                    1_000_000
                    (defaultRetryClient' transactionProxmoxState $ getVMConfig (fromJust proxmoxVMCloneTarget) vmid)
                    vmUnlocked
                  case unlockResult of
                    (Left e)      -> throwError (ClientError e)
                    (Right False) -> throwError (VMLocked proxmoxVMCloneNewID)
                    _             -> pure ()
executeTransactionAction (ConfigureVM vmName payload) = do
  (TransactionState { transactionDeployConfig = deployConfig@(DeployConfig { deployParameters = DeployParams { deployNodeName = nodeName }}),.. }) <- get
  data' <- transactionDataGetF
  case getVMID vmName data' deployConfig of
    Nothing -> $(logWarn) $ T.pack $ "VM " <> vmName <> " has no allocated VMID"
    (Just vmid) -> do
      vmMap <- (defaultRetryClient' transactionProxmoxState) getActiveNodesVMMap >>= defaultClientErrorWrapper
      case M.lookup vmid vmMap of
        Nothing -> $(logWarn) $ T.pack $ "VM with VMID " <> show vmid <> " not found."
        _ -> do
          _ <- defaultRetryClient' transactionProxmoxState (putVMConfig nodeName vmid payload) >>= defaultClientErrorWrapper
          pure ()
executeTransactionAction (TransactionDelayAfter secondsPause action) = do
  () <- executeTransactionAction action
  (liftIO . threadDelay . (* 1_000_000)) secondsPause
executeTransactionAction (DeploySDNNetwork networkCreate@(ProxmoxSDNNetworkCreate { sdnNetworkCreateName = vnetName })) = do
  (TransactionState { transactionDeployConfig = (DeployConfig {deployParameters = (DeployParams { deployNodeName = nodeName }) }),.. }) <- get
  bridgesResponse <- (defaultRetryClient' transactionProxmoxState) (getNodeNetworks nodeName (Just AnyBridge)) >>= defaultClientErrorWrapper
  if sdnNetworkExists vnetName bridgesResponse then
    $(logWarn) $ T.pack $ "SDN network " <> show vnetName <> " already exists"
  else do
    $(logInfo) $ T.pack $ "Creating SDN network " <> show vnetName
    _ <- (defaultRetryClient' transactionProxmoxState) (createSDNNetwork networkCreate) >>= defaultClientErrorWrapper
    $(logInfo) $ T.pack $ "Applying SDN settings"
    _ <- (defaultRetryClient' transactionProxmoxState) applySDNSettings
    bridgeResult <- waitForClient
      60_000_000
      ("SDN network " <> (T.pack . show) vnetName <> " is not created. Waiting...")
      20
      1_000_000
      (defaultRetryClient' transactionProxmoxState $ getNodeNetworks nodeName (Just AnyBridge))
      (sdnNetworkExists vnetName)
    case bridgeResult of
      (Left e) -> throwError (ClientError e)
      (Right True) -> $(logInfo) $ T.pack $ "Created SDN network " <> show vnetName
      (Right False) -> throwError (SDNVnetNotFound vnetName)
executeTransactionAction (DestroySDNNetwork (ProxmoxSDNNetworkCreate { sdnNetworkCreateName = vnetName })) = do
  (TransactionState { transactionDeployConfig = (DeployConfig {deployParameters = (DeployParams { deployNodeName = nodeName }) }),.. }) <- get
  bridgesResponse <- (defaultRetryClient' transactionProxmoxState) (getNodeNetworks nodeName (Just AnyBridge)) >>= defaultClientErrorWrapper
  if not (sdnNetworkExists vnetName bridgesResponse) then
    $(logWarn) $ T.pack $ "SDN network " <> show vnetName <> " does not exists"
  else do
    $(logInfo) $ T.pack $ "Deleting SDN network " <> show vnetName
    _ <- (defaultRetryClient' transactionProxmoxState) (deleteSDNNetwork (T.pack vnetName)) >>= defaultClientErrorWrapper
    $(logInfo) "Applying SDN settings"
    _ <- (defaultRetryClient' transactionProxmoxState) applySDNSettings
    bridgeResult <- waitForClient
      60_000_000
      ("SDN network " <> (T.pack . show) vnetName <> " is existing. Waiting...")
      20
      1_000_000
      (defaultRetryClient' transactionProxmoxState $ getNodeNetworks nodeName (Just AnyBridge))
      (not . sdnNetworkExists vnetName)
    case bridgeResult of
      (Left e) -> throwError (ClientError e)
      (Right True) -> $(logInfo) $ T.pack $ "Deleted SDN network " <> show vnetName
      (Right False) -> throwError (SDNVnetDeleteError vnetName)
executeTransactionAction _ = do
  $(logInfo) "This stage is not supported now."
  pure ()

executeTransaction :: StatefulTransactionT ()
executeTransaction = do
  (TransactionState { .. }) <- get
  case transactionActions of
    [] -> pure ()
    (action:stages) -> do
      $(logInfo) $ T.pack $ "Executing stage " <> show action
      () <- executeTransactionAction action
      oldState <- get
      put (oldState { transactionActions = stages })
      executeTransaction

getVMIDRange :: Int -> [Int] -> [Int]
getVMIDRange startVMID usedVMID = filter (`notElem` usedVMID) [startVMID..999999999]

planTransactionStages :: DeployConfig -> DeployTarget -> [TransactionStage]
planTransactionStages (DeployConfig { deployVMs=vms, deployTemplates=templates, deployNetworks=networks}) target = let
  f :: WriterT [TransactionStage] Identity ()
  f = do
    tell $ map TemplateExists templates
    tell $ map NetworkExists networks
    tell $ map VMExists vms
    let networkCleanVM = nub $ (filter (null . fromJust . configVMNetworks) $ filter (isJust . configVMNetworks) vms) ++ filter configVMCleanNetworks vms
    tell $ map (NetworksRemoved . configVMName) networkCleanVM
    tell $ foldMap generateNetworks vms
    tell $ map (\x -> if configVMRunning x then TS.VMRunning x else TS.VMStopped x) vms
  f' :: WriterT [TransactionStage] Identity ()
  f' = do
    tell $ map TS.VMStopped (reverse vms)
    tell $ map VMNotExists (reverse vms)
    tell $ map NetworkNotExists (reverse networks)

  enumerateNetworks :: [Int] -> [ConfigVMNetwork] -> [ConfigVMNetwork] -> [ConfigVMNetwork]
  enumerateNetworks _ acc [] = reverse acc
  enumerateNetworks takenNumbers acc (net:nets) = do
    let numberRange = filter (`notElem` takenNumbers) [0..31]
    case configVMNetworkNumber net of
      (Just _) -> enumerateNetworks takenNumbers (net:acc) nets
      Nothing -> do
        case take 1 numberRange of
          [] -> error "Network ID overflow!"
          (netID:_) -> enumerateNetworks (netID:takenNumbers) (net { configVMNetworkNumber = Just netID }:acc) nets

  generateNetworks :: ConfigVM -> [TransactionStage]
  generateNetworks (TemplatedConfigVM { configVMName = vmName, configVMNetworks = nets' }) = do
    case nets' of
      Nothing -> []
      (Just nets) -> do
        let enumNets = enumerateNetworks (map (fromJust . configVMNetworkNumber) (filter (isJust . configVMNetworkNumber) nets)) [] nets
        map (NetworkConnected vmName) enumNets
  generateNetworks (RawVM {}) = [] -- TODO: add support
  in (snd . runIdentity . runWriterT) (if target == Deploy then f else f')
--(DeployConfig { deployNetworks = configNetworks, deployTemplates = vmTemplates, deployParameters = DeployParams { deployNodeName = deployNodeName } })
planTransactionActions :: [TransactionStage] -> [ProxmoxNetwork] -> [ProxmoxSDNZone] -> [ProxmoxSDNNetwork] -> [ProxmoxStorage] -> Map Int ProxmoxVM -> TransactionState -> IO (Either TransactionException [TransactionAction])
planTransactionActions stages bridges sdnZones sdnNetworks storages vmMap state' = do
  result <- runExceptT $ (runStateT (unTransaction $ helper stages []) state')
  case result of
    (Left e)             -> (pure . Left) e
    (Right (actions, _)) -> (pure . pure) actions
  where
  helper :: [TransactionStage] -> [TransactionAction] -> StatefulTransactionT [TransactionAction]
  helper [] acc = (pure . reverse) acc
  helper ((NetworkExists (ExistingNetwork networkName)):ts) acc = if any ((==) networkName . proxmoxNetworkInterface) bridges then helper ts acc else
    throwError (BridgeNotFound networkName)
  helper ((NetworkExists SDNNetwork { .. }):ts) acc = do
    let sdnCreate = ProxmoxSDNNetworkCreate
          { sdnNetworkCreateZone=configNetworkZone
          , sdnNetworkCreateVlanaware=Nothing
          , sdnNetworkCreateTag=Nothing
          , sdnNetworkCreateName=configNetworkName
          , sdnNetworkCreateAlias=Nothing
          }
    if all ((/=) configNetworkZone . proxmoxSDNZoneName) sdnZones then throwError (SDNZoneNotFound configNetworkZone) else
      if any (\x -> sdnNetworkName x == configNetworkName && sdnNetworkZone x == configNetworkZone) sdnNetworks then
        helper ts acc
      else do
        let matchingNameNetwork = filter (\x -> sdnNetworkName x == configNetworkName && sdnNetworkZone x /= configNetworkZone) sdnNetworks
        case matchingNameNetwork of
          [] -> helper ts (defaultTransactionDelayAfter (DeploySDNNetwork sdnCreate):acc)
          ((ProxmoxSDNNetwork { sdnNetworkZone = conflictZone }):_) -> do
            helper ts (defaultTransactionDelayAfter (DeploySDNNetwork sdnCreate):defaultTransactionDelayAfter (DestroySDNNetwork (sdnCreate { sdnNetworkCreateZone = conflictZone })):acc)
  helper ((NetworkNotExists (ExistingNetwork {})):ts) acc = helper ts acc
  helper ((NetworkNotExists (SDNNetwork { .. })):ts) acc = do
    if any (\x -> sdnNetworkZone x == configNetworkZone && sdnNetworkName x == configNetworkName) sdnNetworks then do
      let sdnCreate = ProxmoxSDNNetworkCreate
           { sdnNetworkCreateZone = configNetworkZone
           , sdnNetworkCreateName = configNetworkName
           , sdnNetworkCreateVlanaware=Nothing
           , sdnNetworkCreateTag=Nothing
           , sdnNetworkCreateAlias=Nothing
           }
      helper ts (defaultTransactionDelayAfter (DestroySDNNetwork sdnCreate):acc)
    else helper ts acc
  helper ((TemplateExists t@(ConfigTemplate { configTemplateID = tID })):ts) acc = do
    case M.lookup tID vmMap of
      Nothing -> throwError (TemplateNotFound t)
      (Just (ProxmoxVM { vmTemplate=True, vmLock=Nothing })) -> helper ts acc
      _vmInvalid -> throwError (NonTemplateLink t)
  helper ((VMExists vm@(RawVM { configVMID = vmID, configVMName = vmName, configVMDelay = delay })):ts) acc = do
    case vmID of
      Nothing -> helper ts (CreateVM vm:AssignVMID vmName:acc)
      (Just vmID') -> do
        case M.lookup vmID' vmMap of
          (Just _) -> helper ts acc -- TODO: reconfig VM (and unify vm check, wtf)
          Nothing -> do
            helper ts (CreateVM vm:acc)
  helper ((VMExists vm@(TemplatedConfigVM { configVMParentTemplate = parentTemplateName, configVMName = vmName, configVMID = vmID, configVMStorage = vmStorage, configVMDisplay = vmDisplay })):ts) acc = do
    (TransactionState { transactionDeployConfig = deployConfig@(DeployConfig { deployTemplates = vmTemplates, deployParameters = DeployParams { deployNodeName = deployNodeName }, deployAgent = deployAgent }), .. }) <- get
    data' <- transactionDataGetF
    case filter ((==) parentTemplateName . configTemplateName) vmTemplates of
      [] -> throwError (TemplateNotFound (ConfigTemplate {configTemplateName = parentTemplateName, configTemplateID = 0 }))
      (ConfigTemplate { configTemplateID = templateID }:_) -> do
        when (isJust vmStorage) $ do
          case vmStorage of
            Nothing -> pure ()
            (Just storage) -> unless (any ((==storage) . proxmoxStorage) storages) $ throwError (StorageNotFound storage)
        -- if isJust deployAgent && isJust vmDisplay then [SetVMDisplay vmName (fromJust vmDisplay)] else []
        let agentStage = (if isJust deployAgent && isJust vmDisplay then [SetVMDisplay vmName (fromJust vmDisplay)] else [])
        let patchParams = formatConfigVMPatch vm
        let cloneStage = agentStage ++ if isNothing patchParams then [] else [ConfigureVM vmName (fromJust patchParams)] ++ [CloneVM (ProxmoxVMCloneParams {proxmoxVMCloneVMID = templateID, proxmoxVMCloneStorage = fmap T.pack vmStorage, proxmoxVMCloneSnapname = Nothing, proxmoxVMCloneTarget = Just deployNodeName, proxmoxVMCloneNewID = fromMaybe (-1) vmID, proxmoxVMCloneName = (Just . T.pack) vmName, proxmoxVMCloneDescription=Nothing})]
        case getVMID vmName data' deployConfig of
          Nothing -> helper ts (cloneStage ++ (AssignVMID vmName:acc))
          (Just vmID') -> do
            case M.lookup vmID' vmMap of
              (Just _) -> helper ts acc -- TODO: reconfig VM (and unify vm check, wtf)
              Nothing  -> helper ts (cloneStage ++ acc)
  helper ((NetworksRemoved vmName):ts) acc = helper ts (RemoveNetworks vmName:acc)
  helper ((SnapshotExists vmParams snapshotParams@(ProxmoxSnapshotCreate { snapshotCreateName = snapName })):ts) acc = do
    let vmName = configVMName vmParams
    (TransactionState { transactionDeployConfig = deployConfig@(DeployConfig { deployParameters = DeployParams { deployNodeName = deployNodeName }}),  ..}) <- get
    data' <- transactionDataGetF
    case getVMID vmName data' deployConfig of
      Nothing -> helper ts (MakeSnapshot vmName snapshotParams:acc)
      (Just vmID) -> do
        (ProxmoxResponse { proxmoxData = snapshots }) <- (defaultRetryClient' transactionProxmoxState) (getVMSnapshots deployNodeName vmID) >>= defaultClientErrorWrapper
        if any ((==) snapName . snapshotName) snapshots then do
          $(logInfo) $ "Found snapshot " <> snapName <> " of VM " <> T.pack vmName
          helper ts acc
        else do
          helper ts (MakeSnapshot vmName snapshotParams:acc)
  helper ((SnapshotNotExists vmParams snapshotParams@(ProxmoxSnapshotCreate { snapshotCreateName = snapName })):ts) acc = do
    let vmName = configVMName vmParams
    (TransactionState { transactionDeployConfig = deployConfig@(DeployConfig { deployParameters = DeployParams { deployNodeName = deployNodeName }}),  ..}) <- get
    data' <- transactionDataGetF
    case getVMID vmName data' deployConfig of
      Nothing -> helper ts (DeleteSnapshot vmName snapshotParams:acc)
      (Just vmID) -> do
        (ProxmoxResponse { proxmoxData = snapshots }) <- (defaultRetryClient' transactionProxmoxState) (getVMSnapshots deployNodeName vmID) >>= defaultClientErrorWrapper
        if any ((==) snapName . snapshotName) snapshots then do
          $(logInfo) $ "Found snapshot " <> snapName <> " of VM " <> T.pack vmName
          helper ts (DeleteSnapshot vmName snapshotParams:acc)
        else do
          helper ts acc
  helper ((VMRollbacked vmParams targetSnapshot):ts) acc = do
    let vmName = configVMName vmParams
    helper ts (RollbackVM vmName targetSnapshot:acc)
  helper ((NetworkConnected vmName networkConfig@(ConfigVMNetwork { .. })):ts) acc = do
    (TransactionState { transactionDeployConfig = deployConfig@(DeployConfig { deployNetworks = configNetworks, deployParameters = DeployParams { deployNodeName = deployNodeName }}),  ..}) <- get
    let networkNames = map configNetworkName configNetworks
    if configVMNetworkName `notElem` networkNames then throwError (NetworkIsNotDeclared $ configVMNetworkName) else do
      data' <- transactionDataGetF
      case getVMID vmName data' deployConfig of
        Nothing -> helper ts (AttachNetwork vmName networkConfig:acc)
        (Just vmID) -> do
          (ProxmoxResponse { proxmoxData = vmConfig'}) <- (defaultRetryClient' transactionProxmoxState) (getVMConfig deployNodeName vmID) >>= defaultClientErrorWrapper
          case vmConfig' of
            Nothing -> helper ts (AttachNetwork vmName networkConfig:acc)
            (Just vmConfig) -> do
              let bridges = vmNetworkBridges vmConfig
              let networkToRemove = (map fst . filter ((==) configVMNetworkName . snd) . M.toList) bridges
              case configVMNetworkNumber of
                Nothing -> helper ts ([AttachNetwork vmName networkConfig] ++ map (DetachNetwork vmName) networkToRemove ++ acc)
                (Just vmNumber) -> do
                  case M.lookup vmNumber bridges of
                    Nothing -> helper ts ([AttachNetwork vmName networkConfig] ++ map (DetachNetwork vmName) networkToRemove ++ acc)
                    (Just bridgeName) -> if bridgeName == configVMNetworkName then
                      helper ts acc
                    else helper ts ([AttachNetwork vmName networkConfig] ++ map (DetachNetwork vmName) networkToRemove ++ acc)
  helper ((VMNotExists vm):ts) acc = do
    let vmName = configVMName vm
    (TransactionState { transactionDeployConfig = deployConfig@(DeployConfig { deployParameters = DeployParams { deployNodeName = deployNodeName }}),  ..}) <- get
    data' <- transactionDataGetF
    case getVMID vmName data' deployConfig of
      Nothing -> helper ts acc
      (Just vmID) -> do
        (ProxmoxResponse { proxmoxData = vmConfig'}) <- (defaultRetryClient' transactionProxmoxState) (getVMConfig deployNodeName vmID) >>= defaultClientErrorWrapper
        case vmConfig' of
          Nothing -> helper ts acc
          _       -> do
            helper ts (UnassignVMID vmName:DestroyVM vmName:acc)
  helper ((TS.VMStopped vm):ts) acc = do
    actions <- genericPowerFunction VM.VMStopped vm
    helper ts (actions ++ acc)
  helper ((TS.VMRunning vm):ts) acc = do
    actions <- genericPowerFunction VM.VMRunning vm
    helper ts (actions ++ acc)

genericPowerFunction :: VM.ProxmoxVMStatus -> ConfigVM -> StatefulTransactionT [TransactionAction]
genericPowerFunction targetStatus vmConfig = do
  let vmName = configVMName vmConfig
  let vmDelay = configVMDelay vmConfig
  let vmRunning = configVMRunning vmConfig
  (TransactionState { transactionDeployConfig = deployConfig@(DeployConfig { deployParameters = DeployParams { deployNodeName = deployNodeName }}), ..}) <- get
  data' <- transactionDataGetF
  case getVMID vmName data' deployConfig of
    Nothing -> pure [TransactionDelayAfter vmDelay (TS.StartVM vmName) | transactionTarget == Deploy && vmRunning]
    (Just vmID) -> do
      (ProxmoxResponse { proxmoxData = powerRes }) <- (defaultRetryClient' transactionProxmoxState) (getVMPower deployNodeName vmID) >>= defaultClientErrorWrapper
      case powerRes of
        Nothing -> pure [TransactionDelayAfter vmDelay (TS.StartVM vmName) | transactionTarget == Deploy && vmRunning]
        (Just (ProxmoxVMStatusWrapper vmStatus)) -> do
          pure [TransactionDelayAfter vmDelay $ if vmStatus == VM.VMRunning then TS.StopVM vmName else TS.StartVM vmName | vmStatus /= targetStatus]
