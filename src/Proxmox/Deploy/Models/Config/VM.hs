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
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}
module Proxmox.Deploy.Models.Config.VM
  ( ConfigVM(..)
  , ConfigVMNetwork(..)
  , CloudinitAddress(..)
  , isTemplateVM
  , formatConfigVMNetwork
  , formatConfigVMPatch
  ) where

import           Data.Aeson
import qualified Data.Aeson.KeyMap               as KM
import           Data.List                       (intercalate)
import qualified Data.Map                        as M
import           Data.Maybe
import qualified Data.Text                       as T
import           Parsers
import           Proxmox.Models.NetworkInterface
import           Utils

data CloudinitAddress = DHCP | Manual String deriving (Show, Eq, Ord)

instance FromJSON CloudinitAddress where
  parseJSON (String "dhcp") = pure DHCP
  parseJSON (String v)      = (pure . Manual . T.unpack) v
  parseJSON _               = fail "Invalid value for cloudinit address"

instance ToJSON CloudinitAddress where
  toJSON DHCP       = String "dhcp"
  toJSON (Manual v) = (String . T.pack) v

data ConfigVMNetwork = ConfigVMNetwork
  { configVMNetworkName     :: !String
  , configVMNetworkFirewall :: !Bool
  , configVMDeviceType      :: !NetworkInterfaceType
  , configVMNetworkTag      :: !(Maybe Int)
  , configVMNetworkNumber   :: !(Maybe Int)
  , configVMInitAdddress    :: !(Maybe CloudinitAddress)
  , configVMInitGateway     :: !(Maybe String)
  } deriving (Show, Eq, Ord)

formatConfigVMNetwork :: ConfigVMNetwork -> Maybe (String, String)
formatConfigVMNetwork ConfigVMNetwork { .. } = case configVMNetworkNumber of
  Nothing -> Nothing
  (Just netNum) -> Just ("net" <> show netNum, intercalate "," $
    [ "model=" <> show configVMDeviceType
    , "firewall=" <> if configVMNetworkFirewall then "1" else "0"
    , "bridge=" <> configVMNetworkName
    ]
    ++ ["tag=" <> (show . fromJust) configVMNetworkTag | isJust configVMNetworkTag])

instance ToJSON ConfigVMNetwork where
  toJSON (ConfigVMNetwork { .. }) = object
    [ "name" .= configVMNetworkName
    , "firewall" .= configVMNetworkFirewall
    , "type" .= configVMDeviceType
    , "tag" .= configVMNetworkTag
    , "number" .= configVMNetworkNumber
    , "cloudinit_address" .= configVMInitAdddress
    , "cloudinit_gateway" .= configVMInitGateway
    ]

instance FromJSON ConfigVMNetwork where
  parseJSON (String networkName) = pure $ ConfigVMNetwork (T.unpack networkName) True VIRTIO Nothing Nothing Nothing Nothing
  parseJSON (Object v) = ConfigVMNetwork
    <$> v .: "name"
    <*> v .:? "firewall" .!= True
    <*> v .:? "type" .!= VIRTIO
    <*> v .:? "tag"
    <*> nullMaybeWrapper (KM.lookup "number" v) (limitedNumberParser (`elem` [0..31]) "Network number must be in range 0..32")
    <*> v .:? "cloudinit_address"
    <*> v .:? "cloudinit_gateway"
  parseJSON _ = error "ConfigVMNetwork has invalid type"

data ConfigVM = TemplatedConfigVM
  { configVMParentTemplate :: !String
  , configVMName           :: !String
  , configVMID             :: !(Maybe Int)
  , configVMDelay          :: !Int
  , configVMNetworks       :: !(Maybe [ConfigVMNetwork])
  , configVMCleanNetworks  :: !Bool
  , configVMRunning        :: !Bool
  , configVMStorage        :: !(Maybe String)
  , configVMDisplay        :: !(Maybe Int)
  , configVMCores          :: !(Maybe Int)
  , configVMCPULimit       :: !(Maybe Int)
  , configVMMemory         :: !(Maybe Int)
  , configVMTags           :: ![String]
  , configVMInitUser       :: !(Maybe String)
  , configVMInitPassword   :: !(Maybe String)
  , configVMInitUpgrade    :: !Bool
  , configVMInitDNS        :: !(Maybe String)
  , configVMInitDomain     :: !(Maybe String)
  , configVMInitSSHKeys    :: !(Maybe String)
  } | RawVM
  { configVMName         :: !String
  , configVMID           :: !(Maybe Int)
  , configVMDelay        :: !Int
  , configVMRunning      :: !Bool
  , configVMTags         :: ![String]
  , configVMInitUser     :: !(Maybe String)
  , configVMInitPassword :: !(Maybe String)
  , configVMInitUpgrade  :: !Bool
  , configVMInitDNS      :: !(Maybe String)
  , configVMInitDomain   :: !(Maybe String)
  , configVMInitSSHKeys  :: !(Maybe String)
  } deriving (Show, Eq, Ord)

formatConfigVMPatch :: ConfigVM -> Maybe (M.Map String Value)
formatConfigVMPatch RawVM {} = Nothing
formatConfigVMPatch TemplatedConfigVM { configVMCores = Nothing, configVMCPULimit = Nothing, configVMMemory = Nothing } = Nothing
formatConfigVMPatch TemplatedConfigVM { .. } = (Just . M.fromList) $
    cores ++
    limit ++
    memory ++
    tags ++
    networksInit ++
    initUser ++
    initPassword ++
    initUpgrade ++
    initDNS ++
    initDomain ++
    initSSHKeys
    where
  initSSHKeys = maybe [] ((:[]) . ("sshkeys",) . String . T.pack) configVMInitSSHKeys
  initDomain = maybe [] ((:[]) . ("searchdomain",) . String . T.pack) configVMInitDomain
  initUpgrade = ((:[]) . ("ciupgrade",) . String . (\v -> if v then "1" else "0")) configVMInitUpgrade
  initDNS = maybe [] ((:[]) . ("nameserver",) . String . T.pack) configVMInitDNS
  initUser = maybe [] ((:[]) . ("ciuser",) . String . T.pack) configVMInitUser
  initPassword = maybe [] ((:[]) . ("cipassword",) . String . T.pack) configVMInitPassword
  networksInit = foldMap networkInitF (fromMaybe [] configVMNetworks)
  networkInitF :: ConfigVMNetwork -> [(String, Value)]
  networkInitF (ConfigVMNetwork { configVMNetworkNumber = Nothing }) = []
  networkInitF (ConfigVMNetwork { configVMInitAdddress = Nothing }) = []
  networkInitF (ConfigVMNetwork { configVMNetworkNumber = Just netN, configVMInitAdddress = (Just DHCP)}) = [("ipconfig" <> show netN, "ip=dhcp")]
  networkInitF (ConfigVMNetwork { configVMNetworkNumber = Just netN, configVMInitAdddress = (Just (Manual ip)), configVMInitGateway = Nothing }) = [("ipconfig" <> show netN, String $ "ip=" <> T.pack ip)]
  networkInitF (ConfigVMNetwork { configVMNetworkNumber = Just netN, configVMInitAdddress = (Just (Manual ip)), configVMInitGateway = (Just gw)}) = [("ipconfig" <> show netN, String $ "ip=" <> T.pack ip <> ",gw=" <> T.pack gw)]
  cores = case configVMCores of
    Nothing  -> []
    (Just v) -> [("cores", (Number . fromIntegral) v)]
  limit = case configVMCPULimit of
    Nothing  -> []
    (Just v) -> [("cpulimit", (Number . fromIntegral) v)]
  memory = case configVMMemory of
    Nothing  -> []
    (Just v) -> [("memory", (Number . fromIntegral) v)]
  tags = case configVMTags of
    [] -> []
    tagsList -> do
      let escapedTags = filter (not . null) $ map escapeVirtualMachineTag tagsList
      case escapedTags of
        []        -> []
        tagsList' -> [("tags", (String . T.pack) $ intercalate ";" tagsList')]

instance ToJSON ConfigVM where
  toJSON (RawVM { .. }) = object
    [ "name" .= configVMName
    , "vmid" .= configVMID
    , "delay" .= configVMDelay
    , "running" .= configVMRunning
    , "tags" .= configVMTags
    , "cloudinit_user" .= configVMInitUser
    , "cloudinit_password" .= configVMInitPassword
    , "cloudinit_upgrade" .= configVMInitUpgrade
    , "cloudinit_dns" .= configVMInitDNS
    , "cloudinit_domain" .= configVMInitDomain
    , "cloudinit_sshkeys" .= configVMInitSSHKeys
    ]
  toJSON (TemplatedConfigVM { .. }) = object
    [ "clone_from" .= configVMParentTemplate
    , "name" .= configVMName
    , "vmid" .= configVMID
    , "delay" .= configVMDelay
    , "networks" .= configVMNetworks
    , "clean_networks" .= configVMCleanNetworks
    , "running" .= configVMRunning
    , "storage" .= configVMStorage
    , "display" .= configVMDisplay
    , "cores" .= configVMCores
    , "cpu_limit" .= configVMCPULimit
    , "memory" .= configVMMemory
    , "tags" .= configVMTags
    , "cloudinit_user" .= configVMInitUser
    , "cloudinit_password" .= configVMInitPassword
    , "cloudinit_upgrade" .= configVMInitUpgrade
    , "cloudinit_dns" .= configVMInitDNS
    , "cloudinit_domain" .= configVMInitDomain
    , "cloudinit_sshkeys" .= configVMInitSSHKeys
    ]

instance FromJSON ConfigVM where
  parseJSON = withObject "ConfigVM" $ \v -> case KM.lookup "clone_from" v of
    Nothing -> RawVM
      <$> v .: "name"
      <*> v .:? "vmid"
      <*> v .:? "delay" .!= 0
      <*> nullDefaultWrapper (KM.lookup "running" v) True variableBooleanParser
      <*> v .:? "tags" .!= []
      <*> v .:? "cloudinit_user"
      <*> v .:? "cloudinit_password"
      <*> v .:? "cloudinit_upgrade" .!= False
      <*> v .:? "cloudinit_dns"
      <*> v .:? "cloudinit_domain"
      <*> v .:? "cloudinit_sshkeys"
    (Just (String _)) -> TemplatedConfigVM
      <$> v .: "clone_from"
      <*> v .: "name"
      <*> v .:? "vmid"
      <*> v .:? "delay" .!= 0
      <*> v .:? "networks" .!= Nothing
      <*> nullDefaultWrapper (KM.lookup "clean_networks" v) False variableBooleanParser
      <*> nullDefaultWrapper (KM.lookup "running" v) True variableBooleanParser
      <*> v .:? "storage"
      <*> v .:? "display"
      <*> v .:? "cores"
      <*> nullMaybeWrapper (KM.lookup "cpu_limit" v) (limitedNumberParser (`elem` [0..128]) "CPU limit must be in range of 0..128")
      <*> v .:? "memory"
      <*> v .:? "tags" .!= []
      <*> v .:? "cloudinit_user"
      <*> v .:? "cloudinit_password"
      <*> v .:? "cloudinit_upgrade" .!= False
      <*> v .:? "cloudinit_dns"
      <*> v .:? "cloudinit_domain"
      <*> v .:? "cloudinit_sshkeys"
    _anyOtherType -> fail "clone_from field has incorrect value type!"

isTemplateVM :: ConfigVM -> Bool
isTemplateVM (TemplatedConfigVM {}) = True
isTemplateVM _                      = False
