module Printer.Class (MonadPrinter (..)) where

import ConfigLoader.Class (LoadConfigError)
import GetPassword (GetPasswordError)
import LastPass.Class (Password)

class MonadPrinter m where
  printPassword :: Password -> m ()
  printUsage :: m ()
  printError :: GetPasswordError -> m ()
  printLoadConfigError :: LoadConfigError -> m ()
