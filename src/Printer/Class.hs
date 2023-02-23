module Printer.Class (MonadPrinter (..), Printer (..), HasPrinter (..)) where

import App.Error (AppError)
import Control.Monad.Except (ExceptT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (ReaderT, ask)
import Control.Monad.Trans (lift)
import LastPass.Class (Password)

class Monad m => MonadPrinter m where
  printPassword :: Password -> m ()
  printAppError :: AppError -> m ()

data Printer = Printer
  { printPassword_ :: Password -> IO (),
    printAppError_ :: AppError -> IO ()
  }

class HasPrinter env where
  getPrinter :: env -> Printer

instance HasPrinter Printer where
  getPrinter = id

instance (HasPrinter env, MonadIO m) => MonadPrinter (ReaderT env m) where
  printPassword password = do
    env <- ask
    liftIO $ printPassword_ (getPrinter env) password
  printAppError appError = do
    env <- ask
    liftIO $ printAppError_ (getPrinter env) appError

instance MonadPrinter m => MonadPrinter (ExceptT e m) where
  printPassword = lift . printPassword
  printAppError = lift . printAppError
