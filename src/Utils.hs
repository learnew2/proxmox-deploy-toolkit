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
module Utils where

returnFirstDuplicate :: (Eq a) => [a] -> Maybe a
returnFirstDuplicate = helper [] where
  helper :: (Eq a) => [a] -> [a] -> Maybe a
  helper acc (el:els) = if el `elem` acc then Just el else helper (el:acc) els
  helper _ []         = Nothing
