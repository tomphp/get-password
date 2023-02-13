module Printer.Class (MonadPrinter (..)) where

import ConfigLoader.Class (LoadConfigError)
import Control.Monad.Except (ExceptT)
import Control.Monad.Trans (lift)
import Data.Text (Text)
import GetPassword (GetPasswordError)
import LastPass.Class (Password)

class Monad m => MonadPrinter m where
  printPassword :: Password -> m ()
  printGetPasswordError :: GetPasswordError -> m ()
  printLoadConfigError :: LoadConfigError -> m ()
  printUsage :: Text -> m ()

instance MonadPrinter m => MonadPrinter (ExceptT e m) where
  printPassword = lift . printPassword
  printGetPasswordError = lift . printGetPasswordError
  printLoadConfigError = lift . printLoadConfigError
  printUsage = lift . printUsage
