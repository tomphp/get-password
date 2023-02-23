{-# LANGUAGE UndecidableInstances #-}

module Printer.SimplePrinter (simplePrinter) where

import App.Error (AppError (AppGetArgsError, AppGetPasswordError, AppLoadConfigError))
import ConfigLoader.Class (LoadConfigError (LoadConfigError))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Text (Text)
import qualified Data.Text.IO as TextIO
import GetPassword (GetPasswordError (LastPassErrored, MultiplePasswordsFound, NotLoggedIn, PasswordNotFound))
import LastPass.Class (Password (Password))
import LastPass.Entry (Entry (..), EntryID (EntryID))
import LastPass.Error (LastPassError (ListPasswordsFailed, ListPasswordsParseFailed, LoginFailed, NotInstalled, ShowPasswordFailed))
import Printer.Class (Printer (Printer, printAppError_, printPassword_))
import System.IO (stderr)

simplePrinter :: Printer
simplePrinter =
  Printer
    { printPassword_ = printPassword,
      printAppError_ = printAppError
    }

printPassword :: (MonadIO m) => Password -> m ()
printPassword (Password password) = liftIO $ TextIO.putStrLn password

printAppError :: MonadIO m => AppError -> m ()
printAppError (AppGetArgsError progName) = printUsage progName
printAppError (AppLoadConfigError err) = printLoadConfigError err
printAppError (AppGetPasswordError err) = printGetPasswordError err

printLoadConfigError :: MonadIO m => LoadConfigError -> m ()
printLoadConfigError (LoadConfigError err) = liftIO $ TextIO.hPutStrLn stderr ("Config Error: " <> err)

printGetPasswordError :: MonadIO m => GetPasswordError -> m ()
printGetPasswordError PasswordNotFound = liftIO $ TextIO.hPutStrLn stderr "Error: No matching entries found"
printGetPasswordError NotLoggedIn = liftIO $ TextIO.hPutStrLn stderr "Error: Not logged in. Please login with `lpass login`"
printGetPasswordError (MultiplePasswordsFound entries) = do
  liftIO $ TextIO.hPutStrLn stderr "Error: Multiple entries found:"
  liftIO $ TextIO.hPutStrLn stderr "Matching entries:"
  mapM_ printEntry entries
printGetPasswordError (LastPassErrored err) = printLastPassError err

printUsage :: MonadIO m => Text -> m ()
printUsage progName = liftIO $ TextIO.hPutStrLn stderr ("Usage: " <> progName <> " <search>")

printLastPassError :: MonadIO m => LastPassError -> m ()
printLastPassError NotInstalled = liftIO $ TextIO.hPutStrLn stderr "Error: LastPass CLI not installed. Please install with `brew install lastpass-cli`"
printLastPassError LoginFailed = liftIO $ TextIO.hPutStrLn stderr "Error: Failed to login in"
printLastPassError ListPasswordsFailed = liftIO $ TextIO.hPutStrLn stderr "Error: Failed to list passwords"
printLastPassError (ListPasswordsParseFailed _) = liftIO $ TextIO.hPutStrLn stderr "Error: Failed to parse list passwords output"
printLastPassError (ShowPasswordFailed _) = liftIO $ TextIO.hPutStrLn stderr "Error: Failed to show password"

printEntry :: MonadIO m => Entry -> m ()
printEntry Entry {id = EntryID entryID, name} = liftIO $ TextIO.hPutStrLn stderr (" - " <> name <> " [" <> entryID <> "]")
