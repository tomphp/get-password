module Printer.Class (Printer (..), HasPrinter (..), printPassword_, printCopiedMessage_, printAppError_) where

import App.Error (AppError)
import LastPass.Class (Password)
import Lens.Micro.TH (makeClassy)
import RIO

data Printer = Printer
  { _printPassword :: !(forall m. MonadIO m => Password -> m ()),
    _printCopiedMessage :: !(forall m. MonadIO m => m ()),
    _printAppError :: !(forall m. MonadIO m => AppError -> m ())
  }

makeClassy ''Printer

printPassword_ :: (MonadReader env m, HasPrinter env, MonadIO m) => Password -> m ()
printPassword_ password = ask >>= view printPassword <*> pure password

printCopiedMessage_ :: (MonadReader env m, HasPrinter env, MonadIO m) => m ()
printCopiedMessage_ = ask >>= view printCopiedMessage

printAppError_ :: (MonadReader env m, HasPrinter env, MonadIO m) => AppError -> m ()
printAppError_ appError = ask >>= view printAppError <*> pure appError
