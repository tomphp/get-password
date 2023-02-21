module Main (main) where

import ConfigLoader.Class (MonadConfigLoader (loadConfig))
import ConfigLoader.Config (Config (Config, user))
import ConfigLoader.MacLoader (MacLoaderT (runMacLoaderT))
import Control.Monad ((>=>))
import Control.Monad.Except (runExceptT)
import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Data.Text as Text
import GetPassword (GetPasswordError (..))
import qualified GetPassword
import LastPass.Class (MonadLastPass, Password, User)
import qualified LastPass.CliLastPass as LastPass
import LastPass.Entry (Search (Search))
import Printer.Class (MonadPrinter (printError, printLoadConfigError, printPassword, printUsage))
import Printer.SimplePrinter (runSimplePrinterT)
import qualified System.Environment as Env

main :: IO ()
main = do
  runSimplePrinterT $ runMacLoaderT main'

main' :: (MonadConfigLoader m, MonadPrinter m, MonadIO m) => m ()
main' = do
  config <- loadConfig
  case config of
    Left err -> printLoadConfigError err
    Right Config {user} -> do
      args <- liftIO getArgs
      maybe printUsage (LastPass.runCliLastPassT . getPasswordAndPrint user) args

getArgs :: MonadIO m => m (Maybe Search)
getArgs = parseArgs <$> liftIO Env.getArgs

parseArgs :: [String] -> Maybe Search
parseArgs [search] = Just $ Search $ Text.pack search
parseArgs _ = Nothing

getPasswordAndPrint :: (MonadLastPass m, MonadPrinter m, MonadIO m) => Maybe User -> Search -> m ()
getPasswordAndPrint user = runGetPassword user >=> either printError printPassword

runGetPassword :: (MonadIO m, MonadLastPass m) => Maybe User -> Search -> m (Either GetPasswordError Password)
runGetPassword user = runExceptT . GetPassword.getPassword user
