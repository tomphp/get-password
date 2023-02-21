module Printer.SimplePrinter (SimplePrinterT, runSimplePrinterT) where

import ConfigLoader.MacLoader (LoadConfigError (LoadConfigError))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans (MonadTrans (lift))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import GetPassword (GetPasswordError (LastPassErrored, MultiplePasswordsFound, NotLoggedIn, PasswordNotFound))
import LastPass (Entry (Entry, id, name), EntryID (EntryID), LastPassError (ListPasswordsFailed, ListPasswordsParseFailed, LoginFailed, NotInstalled, ShowPasswordFailed), Password (Password))
import Printer.Class (MonadPrinter (printError, printLoadConfigError, printPassword, printUsage))
import qualified System.Environment as Env
import System.IO (stderr)

newtype SimplePrinterT m a = SimplePrinterT {runSimplePrinterT :: m a}
  deriving stock (Functor)
  deriving newtype (Applicative, Monad, MonadIO)

instance MonadTrans SimplePrinterT where
  lift = SimplePrinterT

instance MonadIO m => MonadPrinter (SimplePrinterT m) where
  printPassword = liftIO . printPassword'
  printUsage = liftIO printUsage'
  printError = liftIO . printError'
  printLoadConfigError = liftIO . printLoadConfigError'

printPassword' :: Password -> IO ()
printPassword' (Password password) = TextIO.putStrLn password

printLoadConfigError' :: LoadConfigError -> IO ()
printLoadConfigError' (LoadConfigError err) = putErrorLn ("Config Error: " <> err)

printError' :: GetPasswordError -> IO ()
printError' PasswordNotFound = putErrorLn "Error: No matching entries found"
printError' NotLoggedIn = putErrorLn "Error: Not logged in. Please login with `lpass login`"
printError' (MultiplePasswordsFound entries) = do
  putErrorLn "Error: Multiple entries found:"
  putErrorLn "Matching entries:"
  mapM_ printEntry entries
printError' (LastPassErrored err) = printLastPassError err

printLastPassError :: LastPassError -> IO ()
printLastPassError NotInstalled = putErrorLn "Error: LastPass CLI not installed. Please install with `brew install lastpass-cli`"
printLastPassError LoginFailed = putErrorLn "Error: Failed to login in"
printLastPassError ListPasswordsFailed = putErrorLn "Error: Failed to list passwords"
printLastPassError (ListPasswordsParseFailed _) = putErrorLn "Error: Failed to parse list passwords output"
printLastPassError (ShowPasswordFailed _) = putErrorLn "Error: Failed to show password"

printEntry :: Entry -> IO ()
printEntry Entry {id = EntryID entryID, name} = putErrorLn (" - " <> name <> " [" <> entryID <> "]")

printUsage' :: IO ()
printUsage' = do
  name <- Env.getProgName
  putErrorLn ("Usage: " <> Text.pack name <> " <search>")

putErrorLn :: Text -> IO ()
putErrorLn = TextIO.hPutStrLn stderr
