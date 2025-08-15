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
{-# LANGUAGE RecordWildCards #-}
module Proxmox.Deploy.VM
  ( vmTemplatesPresented
  , validateVMsData
  , VMValidateError(..)
  ) where

import           Proxmox.Deploy.Models.Config.Template
import           Proxmox.Deploy.Models.Config.VM       (ConfigVM (..))
import           Utils                                 (returnFirstDuplicate)

type TemplateName = String
type VMName = String

data VMValidateError = TemplateIsNotPresent TemplateName | TemplateNameRepeat TemplateName | VMNameRepeat VMName

instance Show VMValidateError where
  show (TemplateIsNotPresent templateName) = "Template " <> show templateName <> " is not listed in config file"
  show (TemplateNameRepeat templateName) = "Template name " <> show templateName <> " is used two or more times"
  show (VMNameRepeat vmName) = "VM name " <> show vmName <> " is used two or more times"

validateVMsData :: [ConfigTemplate] -> [ConfigVM] -> Either VMValidateError ()
validateVMsData templates vms = do
  () <- vmTemplatesPresented templates vms
  () <- do
    case returnFirstDuplicate (map configTemplateName templates) of
      Nothing   -> Right ()
      (Just el) -> (Left . TemplateNameRepeat) el
  () <- do
    case returnFirstDuplicate (map configVMName vms) of
      Nothing   -> Right ()
      (Just el) -> (Left . VMNameRepeat) el
  return ()

-- checks, is templates presented on NAME level
-- existence in proxmox node checked earlier
vmTemplatesPresented :: [ConfigTemplate] -> [ConfigVM] -> Either VMValidateError ()
vmTemplatesPresented templates = let
  templatesNames = map configTemplateName templates
  helper :: [ConfigVM] -> Either VMValidateError ()
  helper [] = Right ()
  helper (RawVM {}:vms) = helper vms
  helper (TemplatedConfigVM { .. }:vms) = if configVMParentTemplate `notElem` templatesNames then (Left . TemplateIsNotPresent) configVMParentTemplate else helper vms
  in helper
