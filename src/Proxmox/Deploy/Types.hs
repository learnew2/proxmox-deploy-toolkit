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
{-# LANGUAGE DeriveGeneric #-}
module Proxmox.Deploy.Types (TransactionData(..)) where

import           Data.Aeson
import qualified Data.Map     as M
import           GHC.Generics

data TransactionData = TransactionData
  { transactionIDMap :: !(M.Map String Int)
  } deriving (Show, Eq, Generic)

instance FromJSON TransactionData where

instance ToJSON TransactionData where
