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
module Parsers
  ( intStringParser
  , floatStringParser
  , notNullWrapper
  , nullDefaultWrapper
  , nonEmptyStringParser
  , nullMaybeWrapper
  , variableBooleanParser
  , limitedNumberParser
  ) where

import           Data.Aeson
import           Data.Functor    ((<&>))
import           Data.Scientific
import           Data.Text       (toLower)
import qualified Data.Text       as T
import           Text.Read       (readMaybe)

variableBooleanParser :: MonadFail f => Value -> f Bool
variableBooleanParser (String "1") = pure True
variableBooleanParser (String "0") = pure False
variableBooleanParser (Bool b) = pure b
variableBooleanParser (Number 1) = pure True
variableBooleanParser (Number 0) = pure False
variableBooleanParser (String s) | toLower s `elem` ["true", "yes"] = pure True
                                 | toLower s `elem` ["false", "no"] = pure False
variableBooleanParser _ = error "Invalid boolean value!"

nullMaybeWrapper :: MonadFail f => Maybe Value -> (Value -> f a) -> f (Maybe a)
nullMaybeWrapper Nothing _       = pure Nothing
nullMaybeWrapper (Just Null) _   = pure Nothing
nullMaybeWrapper (Just v) parser = parser v <&> Just

nullDefaultWrapper :: MonadFail f => Maybe Value -> a -> (Value -> f a) -> f a
nullDefaultWrapper Nothing def _     = pure def
nullDefaultWrapper (Just Null) def _ = pure def
nullDefaultWrapper (Just v) _ parser = parser v

notNullWrapper :: MonadFail f => Maybe Value -> (Value -> f a) -> f a
notNullWrapper Nothing _       = fail "Value must be not null"
notNullWrapper (Just Null) _   = fail "Value must be not null"
notNullWrapper (Just v) parser = parser v

nonEmptyStringParser Nothing            = pure Nothing
nonEmptyStringParser (Just Null)        = pure Nothing
nonEmptyStringParser (Just (String "")) = pure Nothing
nonEmptyStringParser (Just (String v))  = (pure . Just . T.unpack) v
nonEmptyStringParser _                  = fail "Invalid value type"

replaceEmptyStringWithNumber :: String -> String
replaceEmptyStringWithNumber "" = "0"
replaceEmptyStringWithNumber v  = v

limitedNumberParser :: MonadFail f => (Int -> Bool) -> String -> Value -> f Int
limitedNumberParser f errorText value= do
  v <- intStringParser value
  if (not . f) v then fail errorText else pure v

intStringParser :: MonadFail f => Value -> f Int
intStringParser (String v) = do
  let vs = (replaceEmptyStringWithNumber . T.unpack) v
  case (readMaybe vs :: Maybe Int) of
    Nothing   -> fail $ "Failed to parse string to int: " <> vs
    (Just vi) -> pure vi
intStringParser (Number v) = case toBoundedInteger v of
  Nothing  -> fail "Failed to convert number"
  (Just i) -> pure i
intStringParser _ = fail "Invalid value type"

floatStringParser :: (Read a, RealFloat a, MonadFail f) => Value -> f a
floatStringParser (String v) = do
  let vs = (replaceEmptyStringWithNumber . T.unpack . T.replace "," ".") v
  case readMaybe vs of
    Nothing  -> fail $ "Failed to parse string to float: " <> vs
    (Just i) -> pure i
floatStringParser (Number v) = pure (toRealFloat v)
floatStringParser _ = fail "Invalid value type"
