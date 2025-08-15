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
module Proxmox.Deploy.Models.Config
  ( DeployConfig(..)
  , decodeDeployConfig
  , updateDeployConfigToken
  , emptyDeployConfig
  ) where

import           Data.Aeson
import           Data.Text                                (Text)
import qualified Data.Yaml                                as Y
import           Proxmox.Deploy.Models.Config.Deploy
import           Proxmox.Deploy.Models.Config.DeployAgent
import           Proxmox.Deploy.Models.Config.Network
import           Proxmox.Deploy.Models.Config.Template
import           Proxmox.Deploy.Models.Config.VM

data DeployConfig = DeployConfig
  { deployTemplates  :: ![ConfigTemplate]
  , deployParameters :: !DeployParams
  , deployVMs        :: ![ConfigVM]
  , deployNetworks   :: ![ConfigNetwork]
  , deployAgent      :: !(Maybe DeployAgentConfig)
  } deriving Show

instance FromJSON DeployConfig where
  parseJSON = withObject "DeployConfig" $ \v -> DeployConfig
    <$> v .:? "templates" .!= []
    <*> v .: "deploy"
    <*> v .:? "vms" .!= []
    <*> v .:? "networks" .!= []
    <*> v .:? "agent"

decodeDeployConfig :: FilePath -> IO (Either Y.ParseException DeployConfig)
decodeDeployConfig = Y.decodeFileEither

updateDeployConfigToken :: Maybe Text -> DeployConfig -> DeployConfig
updateDeployConfigToken (Just token) c@(DeployConfig {deployParameters = p@(DeployParams {deployToken = Nothing })}) = c { deployParameters = p { deployToken = Just token } }
updateDeployConfigToken _ p = p

-- used for tests, at least now
emptyDeployConfig :: DeployConfig
emptyDeployConfig = DeployConfig {deployTemplates=[], deployParameters=DeployParams {deployUrl="", deployToken=Nothing, deployNodeName="", deployIgnoreSSL=False, deployStartVMID = 100}, deployVMs=[], deployNetworks=[], deployAgent=Nothing}
