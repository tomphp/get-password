module Printer.Class (Printer (..), HasPrinter (..), printPassword_, printAppError_) where

import App.Error (AppError)
import LastPass.Class (Password)
import Lens.Micro.TH (makeClassy)
import RIO

data Printer = Printer
  { _printPassword :: !(forall m. MonadIO m => Password -> m ()),
    _printAppError :: !(forall m. MonadIO m => AppError -> m ())
  }

makeClassy ''Printer

printPassword_ :: (MonadReader env m, HasPrinter env, MonadIO m) => Password -> m ()
printPassword_ password = ask >>= view printPassword <*> pure password

printAppError_ :: (MonadReader env m, HasPrinter env, MonadIO m) => AppError -> m ()
printAppError_ appError = ask >>= view printAppError <*> pure appError
