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
module Proxmox.Deploy.Models.Config.Network
  ( ConfigNetwork(..)
  ) where

import           Control.Applicative
import           Data.Aeson
import qualified Data.Aeson.KeyMap   as KV

data ConfigSubnetRange = ConfigSubnetRange
  { configSubnetRangeStart :: !String
  , configSubnetRangeEnd   :: !String
  } deriving (Show, Eq)

instance FromJSON ConfigSubnetRange where
  parseJSON = withObject "ConfigSubnetRange" $ \v -> ConfigSubnetRange
    <$> (v .: "start" <|> v .: "begin")
    <*> v .: "end"

data ConfigSubnet = ConfigSubnet
  { configSubnetCIDR      :: !String
  , configSubnetDNS       :: !(Maybe String)
  , configSubnetDNSPrefix :: !(Maybe String)
  , configSubnetGateway   :: !(Maybe String)
  , configSubnetSNAT      :: !Bool
  , configSubnetRanges    :: ![ConfigSubnetRange]
  } deriving (Show, Eq)

instance FromJSON ConfigSubnet where
  parseJSON = withObject "ConfigSubnet" $ \v -> ConfigSubnet
    <$> v .: "cidr"
    <*> v .:? "dns"
    <*> v .:? "dns_prefix"
    <*> v .:? "gateway"
    <*> v .:? "snat" .!= False
    <*> v .:? "dhcp_ranges" .!= []

data ConfigNetwork = ExistingNetwork
  { configNetworkName :: !String
  } |
  SDNNetwork
  { configNetworkSubnets   :: ![ConfigSubnet]
  , configNetworkZone      :: !String
  , configNetworkName      :: !String
  , configNetworkVLANAware :: !(Maybe Bool)
  } deriving (Show, Eq)

instance FromJSON ConfigNetwork where
  parseJSON = let
    existingNetworkParser v = ExistingNetwork
      <$> v .: "name"
    sdnNetworkParser v = SDNNetwork
      <$> v .:? "subnets" .!= []
      <*> v .: "zone"
      <*> v .: "name"
      <*> v .:? "vlanaware"
    in withObject "ConfigNetwork" $ \v -> case KV.lookup "type" v of
    Nothing           -> existingNetworkParser v
    (Just "existing") -> existingNetworkParser v
    (Just "sdn")      -> sdnNetworkParser v
    _anyOther         -> fail $ "Invalid network type value: " <> show _anyOther
