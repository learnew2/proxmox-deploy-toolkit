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
module Proxmox.Deploy.Models.Config.Deploy
  ( DeployParams(..)
  ) where

import           Data.Aeson
import           Data.Text

data DeployParams = DeployParams
  { deployNodeName  :: !Text
  , deployToken     :: !(Maybe Text)
  , deployUrl       :: !Text
  , deployIgnoreSSL :: !Bool
  , deployStartVMID :: !Int
  } deriving (Show, Eq, Ord)

instance FromJSON DeployParams where
  parseJSON = withObject "DeployParams" $ \v -> DeployParams
    <$> v .: "node"
    <*> v .:? "token"
    <*> v .: "url"
    <*> v .:? "ignore_ssl" .!= False
    <*> v .:? "start_vmid" .!= 100

instance ToJSON DeployParams where
  toJSON (DeployParams { .. }) = object
    [ "node" .= deployNodeName
    , "token" .= deployToken
    , "url" .= deployUrl
    , "ignore_ssl" .= deployIgnoreSSL
    , "start_vmid" .= deployStartVMID
    ]
