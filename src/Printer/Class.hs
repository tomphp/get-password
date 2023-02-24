module Printer.Class (MonadPrinter (..), Printer (..), HasPrinter (..)) where

import App.Error (AppError)
import Control.Monad.Except (ExceptT)
import LastPass.Class (Password)
import Lens.Micro.TH (makeClassy)
import RIO

class Monad m => MonadPrinter m where
  printPassword_ :: Password -> m ()
  printAppError_ :: AppError -> m ()

data Printer = Printer
  { _printPassword :: !(forall m. MonadIO m => Password -> m ()),
    _printAppError :: !(forall m. MonadIO m => AppError -> m ())
  }

makeClassy ''Printer

instance (HasPrinter env, MonadIO m) => MonadPrinter (ReaderT env m) where
  printPassword_ password = do
    env <- ask
    liftIO $ _printPassword (env ^. printer) password
  printAppError_ appError = do
    env <- ask
    liftIO $ _printAppError (env ^. printer) appError

instance MonadPrinter m => MonadPrinter (ExceptT e m) where
  printPassword_ = lift . printPassword_
  printAppError_ = lift . printAppError_
