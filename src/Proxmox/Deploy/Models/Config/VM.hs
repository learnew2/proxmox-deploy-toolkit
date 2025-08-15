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
module Proxmox.Deploy.Models.Config.VM
  ( ConfigVM(..)
  , ConfigVMNetwork(..)
  , isTemplateVM
  , formatConfigVMNetwork
  ) where

import           Data.Aeson
import qualified Data.Aeson.KeyMap               as KM
import           Data.List                       (intercalate)
import           Data.Maybe
import qualified Data.Text                       as T
import           Parsers
import           Proxmox.Models.NetworkInterface

data ConfigVMNetwork = ConfigVMNetwork
  { configVMNetworkName     :: !String
  , configVMNetworkFirewall :: !Bool
  , configVMDeviceType      :: !NetworkInterfaceType
  , configVMNetworkTag      :: !(Maybe Int)
  , configVMNetworkNumber   :: !(Maybe Int)
  } deriving (Show, Eq)

formatConfigVMNetwork :: ConfigVMNetwork -> Maybe (String, String)
formatConfigVMNetwork ConfigVMNetwork { .. } = case configVMNetworkNumber of
  Nothing -> Nothing
  (Just netNum) -> Just ("net" <> show netNum, intercalate "," $
    [ "model=" <> show configVMDeviceType
    , "firewall=" <> if configVMNetworkFirewall then "1" else "0"
    , "bridge=" <> configVMNetworkName
    ]
    ++ ["tag=" <> (show . fromJust) configVMNetworkTag | isJust configVMNetworkTag])

instance FromJSON ConfigVMNetwork where
  parseJSON (String networkName) = pure $ ConfigVMNetwork (T.unpack networkName) True VIRTIO Nothing Nothing
  parseJSON (Object v) = ConfigVMNetwork
    <$> v .: "name"
    <*> v .:? "firewall" .!= True
    <*> v .:? "type" .!= VIRTIO
    <*> v .:? "tag"
    <*> nullMaybeWrapper (KM.lookup "number" v) (limitedNumberParser (`elem` [0..31]) "Network number must be in range 0..32")
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
  } | RawVM
  { configVMName    :: !String
  , configVMID      :: !(Maybe Int)
  , configVMDelay   :: !Int
  , configVMRunning :: !Bool
  } deriving (Show, Eq)

instance FromJSON ConfigVM where
  parseJSON = withObject "ConfigVM" $ \v -> case KM.lookup "clone_from" v of
    Nothing -> RawVM
      <$> v .: "name"
      <*> v .:? "vmid"
      <*> v .:? "delay" .!= 0
      <*> nullDefaultWrapper (KM.lookup "running" v) True variableBooleanParser
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
    _anyOtherType -> fail "clone_from field has incorrect value type!"

isTemplateVM :: ConfigVM -> Bool
isTemplateVM (TemplatedConfigVM {}) = True
isTemplateVM _                      = False
