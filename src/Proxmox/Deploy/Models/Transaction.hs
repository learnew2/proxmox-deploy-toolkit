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
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
module Proxmox.Deploy.Models.Transaction
  ( TransactionStage(..)
  , TransactionAction(..)
  , TransactionException(..)
  , TransactionState(..)
  , StatefulTransactionT(..)
  , DeployTarget(..)
  , defaultClientErrorWrapper
  , defaultTransactionDelayAfter
  ) where

import           Control.Monad.Catch
import           Control.Monad.Except
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Control.Monad.State                   (MonadState, get, gets)
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.State             (StateT)
import           Data.Aeson
import qualified Data.Map                              as M
import           Proxmox.Deploy.Models.Config
import           Proxmox.Deploy.Models.Config.Network
import           Proxmox.Deploy.Models.Config.Template
import           Proxmox.Deploy.Models.Config.VM
import           Proxmox.Deploy.Types
import           Proxmox.Models.SDNNetwork
import           Proxmox.Models.VMClone
import           Proxmox.Schema
import           Servant.Client

data TransactionStage
  = NetworkExists ConfigNetwork
  | NetworkNotExists ConfigNetwork
  | VMExists ConfigVM
  | VMNotExists ConfigVM
  | TemplateExists ConfigTemplate
  | NetworkConnected String ConfigVMNetwork
  | NetworksRemoved String
  | VMStopped ConfigVM
  | VMRunning ConfigVM
  deriving (Show, Eq)

data TransactionAction
  = DeploySDNNetwork ProxmoxSDNNetworkCreate
  | DestroySDNNetwork ProxmoxSDNNetworkCreate
  | UnassignVMID String
  | AssignVMID String
  | CloneVM ProxmoxVMCloneParams
  | ConfigureVM String (M.Map String Value)
  | SetVMDisplay String Int
  | CreateVM ConfigVM -- replace
  | DestroyVM String
  | StopVM String
  | StartVM String
  | RemoveNetworks String
  | AttachNetwork String ConfigVMNetwork
  | TransactionDelayAfter Int TransactionAction
  | DetachNetwork String Int
  deriving (Show, Eq)

data DeployTarget = Deploy | Destroy deriving (Show, Eq)

data TransactionException = BridgeNotFound String
  | SDNZoneNotFound String
  | SDNVnetNotFound String
  | SDNVnetDeleteError String
  | TemplateNotFound ConfigTemplate
  | TemplateNodeNotFound ConfigTemplate
  | NonTemplateLink ConfigTemplate
  | FileError String
  | ClientError ClientError
  | MachineHasNoID String
  | VMDeleteError Int
  | VMLocked Int
  | UnknownError String
  | VMIDTaken Int
  | NetworkIsNotDeclared String
  | VMConfigIsNotFound String
  | StorageNotFound String deriving Show

defaultTransactionDelayAfter :: TransactionAction -> TransactionAction
defaultTransactionDelayAfter = TransactionDelayAfter 5

data TransactionState = TransactionState
  { transactionAllocateVMIDF :: StatefulTransactionT Int
  , transactionDataGetF      :: StatefulTransactionT TransactionData
  , transactionDataSetF      :: TransactionData -> StatefulTransactionT ()
  , transactionActions       :: ![TransactionAction]
  , transactionDeployConfig  :: !DeployConfig
  , transactionProxmoxState  :: !ProxmoxState
  , transactionTarget        :: !DeployTarget
  , transactionLogFunction :: !(Loc -> LogSource -> LogLevel -> LogStr -> IO ())
  }

newtype StatefulTransactionT a = StatefulTransactionT { unTransaction :: StateT TransactionState (ExceptT TransactionException IO) a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadState TransactionState, MonadError TransactionException)

instance MonadThrow StatefulTransactionT where
  throwM = (throwError . UnknownError . show)

instance MonadLogger StatefulTransactionT where
  monadLoggerLog loc src level msg = do
    f <- gets transactionLogFunction
    liftIO $ f loc src level (toLogStr msg)

instance MonadLoggerIO StatefulTransactionT where
  askLoggerIO = gets transactionLogFunction

defaultClientErrorWrapper :: Either ClientError a -> StatefulTransactionT a
defaultClientErrorWrapper (Left e)  = throwError (ClientError e)
defaultClientErrorWrapper (Right v) = pure v
