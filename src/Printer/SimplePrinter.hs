module Printer.SimplePrinter (simplePrinter) where

import App.Error (AppError (AppGetArgsError, AppGetPasswordError, AppLoadConfigError))
import ConfigLoader.Class (LoadConfigError (LoadConfigError))
import GetPassword (GetPasswordError (LastPassErrored, MultiplePasswordsFound, NotLoggedIn, PasswordNotFound))
import LastPass.Class (Password (Password))
import LastPass.Entry (Entry (..), EntryID (EntryID))
import LastPass.Error (LastPassError (CopyPasswordFailed, ListPasswordsFailed, ListPasswordsParseFailed, LoginFailed, NotInstalled, ShowPasswordFailed))
import Printer.Class (Printer (..))
import RIO
import qualified RIO.Text as Text
import System.IO (hPutStrLn, putStrLn)

simplePrinter :: Printer
simplePrinter =
  Printer
    { _printPassword = printPassword,
      _printCopiedMessage = printCopiedMessage,
      _printAppError = printAppError
    }

printPassword :: (MonadIO m) => Password -> m ()
printPassword (Password password) = liftIO $ putStrLn (Text.unpack password)

printCopiedMessage :: (MonadIO m) => m ()
printCopiedMessage = liftIO $ putStrLn "Password copied to clipboard!"

printAppError :: MonadIO m => AppError -> m ()
printAppError (AppGetArgsError progName) = printUsage progName
printAppError (AppLoadConfigError err) = printLoadConfigError err
printAppError (AppGetPasswordError err) = printGetPasswordError err

printLoadConfigError :: MonadIO m => LoadConfigError -> m ()
printLoadConfigError (LoadConfigError err) = liftIO $ hPutStrLn stderr ("Config Error: " <> Text.unpack err)

printGetPasswordError :: MonadIO m => GetPasswordError -> m ()
printGetPasswordError PasswordNotFound = liftIO $ hPutStrLn stderr "Error: No matching entries found"
printGetPasswordError NotLoggedIn = liftIO $ hPutStrLn stderr "Error: Not logged in. Please login with `lpass login`"
printGetPasswordError (MultiplePasswordsFound entries) = do
  liftIO $ hPutStrLn stderr "Error: Multiple entries found:"
  liftIO $ hPutStrLn stderr "Matching entries:"
  mapM_ printEntry entries
printGetPasswordError (LastPassErrored err) = printLastPassError err

printUsage :: MonadIO m => Text -> m ()
printUsage progName = liftIO $ hPutStrLn stderr ("Usage: " <> Text.unpack progName <> " <search>")

printLastPassError :: MonadIO m => LastPassError -> m ()
printLastPassError NotInstalled = liftIO $ hPutStrLn stderr "Error: LastPass CLI not installed. Please install with `brew install lastpass-cli`"
printLastPassError LoginFailed = liftIO $ hPutStrLn stderr "Error: Failed to login in"
printLastPassError ListPasswordsFailed = liftIO $ hPutStrLn stderr "Error: Failed to list passwords"
printLastPassError (ListPasswordsParseFailed _) = liftIO $ hPutStrLn stderr "Error: Failed to parse list passwords output"
printLastPassError (ShowPasswordFailed _) = liftIO $ hPutStrLn stderr "Error: Failed to show password"
printLastPassError (CopyPasswordFailed _) = liftIO $ hPutStrLn stderr "Error: Failed to copy password"

printEntry :: MonadIO m => Entry -> m ()
printEntry Entry {id = EntryID entryID, name} = liftIO $ hPutStrLn stderr (" - " <> Text.unpack name <> " [" <> Text.unpack entryID <> "]")
