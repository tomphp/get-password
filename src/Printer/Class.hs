module Printer.Class (MonadPrinter (..)) where

import Config.Loader (LoadConfigError)
import GetPassword (GetPasswordError)
import LastPass (Password)

class MonadPrinter m where
  printPassword :: Password -> m ()
  printUsage :: m ()
  printError :: GetPasswordError -> m ()
  printLoadConfigError :: LoadConfigError -> m ()
