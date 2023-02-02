module Printer.Class (MonadPrinter (..)) where

import App.Error (AppError)
import Control.Monad.Except (ExceptT)
import Control.Monad.Trans (lift)
import LastPass.Class (Password)

class Monad m => MonadPrinter m where
  printPassword :: Password -> m ()
  printAppError :: AppError -> m ()

instance MonadPrinter m => MonadPrinter (ExceptT e m) where
  printPassword = lift . printPassword
  printAppError = lift . printAppError
