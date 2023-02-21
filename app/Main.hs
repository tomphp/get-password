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
main = runSimplePrinterT $ runMacLoaderT $ LastPass.runCliLastPassT main'

main' :: (MonadLastPass m, MonadConfigLoader m, MonadPrinter m, MonadIO m) => m ()
main' = loadConfig >>= either printLoadConfigError runGetPassword

runGetPassword :: (MonadLastPass m, MonadPrinter m, MonadIO m) => Config -> m ()
runGetPassword Config {user} = liftIO getArgs >>= maybe printUsage (getPasswordAndPrint user)

getArgs :: MonadIO m => m (Maybe Search)
getArgs = parseArgs <$> liftIO Env.getArgs

parseArgs :: [String] -> Maybe Search
parseArgs [search] = Just $ Search $ Text.pack search
parseArgs _ = Nothing

getPasswordAndPrint :: (MonadLastPass m, MonadPrinter m, MonadIO m) => Maybe User -> Search -> m ()
getPasswordAndPrint user = getPassword user >=> either printError printPassword

getPassword :: (MonadIO m, MonadLastPass m) => Maybe User -> Search -> m (Either GetPasswordError Password)
getPassword user = runExceptT . GetPassword.getPassword user
