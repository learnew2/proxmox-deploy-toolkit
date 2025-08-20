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
module Proxmox.Deploy.Models.Config.DeployAgent
  ( DeployAgentConfig(..)
  ) where

import           Data.Aeson
import           Data.Text

data DeployAgentConfig = DeployAgentConfig
  { configAgentURL            :: !Text
  , configAgentToken          :: !Text
  , configAgentDisplayNetwork :: !Text
  } deriving (Show, Eq, Ord)

instance FromJSON DeployAgentConfig where
  parseJSON = withObject "DeployAgentConfig" $ \v -> DeployAgentConfig
    <$> v .: "url"
    <*> v .: "token"
    <*> v .:? "display_network" .!= "0.0.0.0"

instance ToJSON DeployAgentConfig where
  toJSON (DeployAgentConfig { .. }) = object
    [ "url" .= configAgentURL
    , "token" .= configAgentToken
    , "display_network" .= configAgentDisplayNetwork
    ]
