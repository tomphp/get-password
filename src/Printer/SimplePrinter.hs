module Printer.SimplePrinter (SimplePrinterT, runSimplePrinterT) where

import App.Error (AppError (AppGetArgsError, AppGetPasswordError, AppLoadConfigError))
import ConfigLoader.Class (LoadConfigError (LoadConfigError), MonadConfigLoader)
import Console.Class (MonadConsole)
import qualified Console.Class as Console
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans (MonadTrans (lift))
import Data.Text (Text)
import GetPassword (GetPasswordError (LastPassErrored, MultiplePasswordsFound, NotLoggedIn, PasswordNotFound))
import LastPass.Class (Password (Password))
import LastPass.Entry (Entry (..), EntryID (EntryID))
import LastPass.Error (LastPassError (ListPasswordsFailed, ListPasswordsParseFailed, LoginFailed, NotInstalled, ShowPasswordFailed))
import Printer.Class (MonadPrinter (printAppError, printPassword))

newtype SimplePrinterT m a = SimplePrinterT {runSimplePrinterT :: m a}
  deriving stock (Functor)
  deriving newtype (Applicative, Monad, MonadIO, MonadConsole, MonadConfigLoader)

instance MonadTrans SimplePrinterT where
  lift = SimplePrinterT

instance (Monad m, MonadConsole m) => MonadPrinter (SimplePrinterT m) where
  printPassword = printPassword'
  printAppError = lift . printAppError'

printPassword' :: MonadConsole m => Password -> m ()
printPassword' (Password password) = Console.printLine password

printAppError' :: MonadConsole m => AppError -> m ()
printAppError' (AppGetArgsError progName) = printUsage progName
printAppError' (AppLoadConfigError err) = printLoadConfigError err
printAppError' (AppGetPasswordError err) = printGetPasswordError err

printLoadConfigError :: MonadConsole m => LoadConfigError -> m ()
printLoadConfigError (LoadConfigError err) = Console.printErrorLine ("Config Error: " <> err)

printGetPasswordError :: (Monad m, MonadConsole m) => GetPasswordError -> m ()
printGetPasswordError PasswordNotFound = Console.printErrorLine "Error: No matching entries found"
printGetPasswordError NotLoggedIn = Console.printErrorLine "Error: Not logged in. Please login with `lpass login`"
printGetPasswordError (MultiplePasswordsFound entries) = do
  Console.printErrorLine "Error: Multiple entries found:"
  Console.printErrorLine "Matching entries:"
  mapM_ printEntry entries
printGetPasswordError (LastPassErrored err) = printLastPassError err

printUsage :: MonadConsole m => Text -> m ()
printUsage progName = Console.printErrorLine ("Usage: " <> progName <> " <search>")

printLastPassError :: MonadConsole m => LastPassError -> m ()
printLastPassError NotInstalled = Console.printErrorLine "Error: LastPass CLI not installed. Please install with `brew install lastpass-cli`"
printLastPassError LoginFailed = Console.printErrorLine "Error: Failed to login in"
printLastPassError ListPasswordsFailed = Console.printErrorLine "Error: Failed to list passwords"
printLastPassError (ListPasswordsParseFailed _) = Console.printErrorLine "Error: Failed to parse list passwords output"
printLastPassError (ShowPasswordFailed _) = Console.printErrorLine "Error: Failed to show password"

printEntry :: MonadConsole m => Entry -> m ()
printEntry Entry {id = EntryID entryID, name} = Console.printErrorLine (" - " <> name <> " [" <> entryID <> "]")
