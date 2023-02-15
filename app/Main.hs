module Main (main) where

import qualified Args
import ConfigLoader.Class (MonadConfigLoader (loadConfig))
import ConfigLoader.Config (Config (Config, user))
import ConfigLoader.MacLoader (MacLoaderT (runMacLoaderT))
import Console.IOConsole (runIOConsole)
import Control.Monad ((>=>))
import Control.Monad.Except (runExceptT)
import Control.Monad.IO.Class (MonadIO)
import GetPassword (GetPasswordError (..))
import qualified GetPassword
import LastPass.Class (MonadLastPass, Password, User)
import qualified LastPass.CliLastPass as LastPass
import LastPass.Entry (Search)
import Printer.Class (MonadPrinter (printError, printLoadConfigError, printPassword))
import Printer.SimplePrinter (runSimplePrinterT)

main :: IO ()
main = do
  search <- runIOConsole Args.getArgs
  runSimplePrinterT $ runMacLoaderT $ LastPass.runCliLastPassT $ main' search

main' :: (MonadLastPass m, MonadConfigLoader m, MonadPrinter m, MonadIO m) => Search -> m ()
main' search = loadConfig >>= either printLoadConfigError (runGetPassword search)

runGetPassword :: (MonadLastPass m, MonadPrinter m, MonadIO m) => Search -> Config -> m ()
runGetPassword search Config {user} = getPasswordAndPrint user search

getPasswordAndPrint :: (MonadLastPass m, MonadPrinter m, MonadIO m) => Maybe User -> Search -> m ()
getPasswordAndPrint user = getPassword user >=> either printError printPassword

getPassword :: (MonadIO m, MonadLastPass m) => Maybe User -> Search -> m (Either GetPasswordError Password)
getPassword user = runExceptT . GetPassword.getPassword user
