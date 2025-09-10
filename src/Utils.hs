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

import           Data.Char

transliterateCharacter :: Char -> String
transliterateCharacter 'а' = "a"
transliterateCharacter 'б' = "b"
transliterateCharacter 'в' = "v"
transliterateCharacter 'г' = "g"
transliterateCharacter 'д' = "d"
transliterateCharacter 'е' = "e"
transliterateCharacter 'ё' = "e"
transliterateCharacter 'ж' = "zh"
transliterateCharacter 'з' = "z"
transliterateCharacter 'и' = "i"
transliterateCharacter 'й' = "i"
transliterateCharacter 'к' = "k"
transliterateCharacter 'л' = "l"
transliterateCharacter 'м' = "m"
transliterateCharacter 'н' = "n"
transliterateCharacter 'о' = "o"
transliterateCharacter 'п' = "p"
transliterateCharacter 'р' = "r"
transliterateCharacter 'с' = "s"
transliterateCharacter 'т' = "t"
transliterateCharacter 'у' = "y"
transliterateCharacter 'ф' = "f"
transliterateCharacter 'х' = "h"
transliterateCharacter 'ц' = "c"
transliterateCharacter 'ч' = "ch"
transliterateCharacter 'ш' = "sh"
transliterateCharacter 'щ' = "sch"
transliterateCharacter 'ъ' = ""
transliterateCharacter 'ы' = "i"
transliterateCharacter 'ь' = ""
transliterateCharacter 'э' = "e"
transliterateCharacter 'ю' = "yu"
transliterateCharacter 'я' = "ya"
transliterateCharacter ' '   = "_"
transliterateCharacter s | isLetter s || isNumber s = [s]
                         | otherwise = []

virtualMachineTagSymbols = ['a'..'z'] ++ ['0'..'9'] ++ ['_']

escapeVirtualMachineTag :: String -> String
escapeVirtualMachineTag = take 150 . filter (`elem` virtualMachineTagSymbols) . foldMap (transliterateCharacter . toLower)

returnFirstDuplicate :: (Eq a) => [a] -> Maybe a
returnFirstDuplicate = helper [] where
  helper :: (Eq a) => [a] -> [a] -> Maybe a
  helper acc (el:els) = if el `elem` acc then Just el else helper (el:acc) els
  helper _ []         = Nothing
