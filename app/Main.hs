module Main (main) where

import Config.Config (Config (Config, user))
import Config.Loader (ConfigLoader (loadConfig))
import Config.MacLoader (MacLoaderT (runMacLoaderT))
import Control.Monad ((>=>))
import Control.Monad.Except (runExceptT)
import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Data.Text as Text
import GetPassword (GetPasswordError (..))
import qualified GetPassword
import LastPass (MonadLastPass, Password, Search (Search), User)
import qualified LastPass
import Printer.Class (MonadPrinter (printError, printLoadConfigError, printPassword, printUsage))
import Printer.SimplePrinter (runSimplePrinterT)
import qualified System.Environment as Env

main :: IO ()
main = do
  runSimplePrinterT $ runMacLoaderT main'

main' :: (ConfigLoader m, MonadPrinter m, MonadIO m) => m ()
main' = do
  config <- loadConfig
  case config of
    Left err -> printLoadConfigError err
    Right Config {user} -> do
      args <- liftIO getArgs
      maybe printUsage (getPasswordAndPrint user) args

getArgs :: MonadIO m => m (Maybe Search)
getArgs = parseArgs <$> liftIO Env.getArgs

parseArgs :: [String] -> Maybe Search
parseArgs [search] = Just $ Search $ Text.pack search
parseArgs _ = Nothing

getPasswordAndPrint :: (MonadPrinter m, MonadIO m) => Maybe User -> Search -> m ()
getPasswordAndPrint user = LastPass.runCliLastPassT . runGetPassword user >=> either printError printPassword

runGetPassword :: (MonadIO m, MonadLastPass m) => Maybe User -> Search -> m (Either GetPasswordError Password)
runGetPassword user = runExceptT . GetPassword.getPassword user
