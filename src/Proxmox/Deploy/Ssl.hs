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
module Proxmox.Deploy.Ssl
  ( createProxmoxManager
  , createSSLManager
  ) where

import qualified Data.ByteString.Char8               as BS
import           Data.CaseInsensitive
import           Data.Text                           (unpack)
import           Network.Connection
import           Network.HTTP.Client
import           Network.HTTP.Conduit
import           Proxmox.Deploy.Models.Config
import           Proxmox.Deploy.Models.Config.Deploy

createSSLManager :: DeployConfig -> IO Manager
createSSLManager (DeployConfig { deployParameters=DeployParams {deployToken=token, deployIgnoreSSL=ignoreSSL}}) = let
  tlsSettings = TLSSettingsSimple
    { settingDisableCertificateValidation = ignoreSSL
    , settingDisableSession = False
    , settingUseServerName = True
    }

  in do
  let settings = mkManagerSettings tlsSettings Nothing
  newManager settings

createProxmoxManager :: DeployConfig -> IO Manager
createProxmoxManager (DeployConfig { deployParameters=DeployParams {deployToken=token, deployIgnoreSSL=ignoreSSL}}) = let
  f :: Request -> IO Request
  f r = do
    case token of
      Nothing -> return r
      (Just tokenValue) -> do
        return (r { requestHeaders = filter ((/= "Authorization") . fst) (requestHeaders r) ++ [(mk $ BS.pack "Authorization", BS.pack $ "PVEAPIToken=" <> unpack tokenValue)]})

  tlsSettings = TLSSettingsSimple
    { settingDisableCertificateValidation = ignoreSSL
    , settingDisableSession = False
    , settingUseServerName = True
    }

  in do
  let settings = mkManagerSettings tlsSettings Nothing
  newManager (settings { managerModifyRequest = f })
