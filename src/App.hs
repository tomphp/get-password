module App (app, AppError (..)) where

import App.Error (AppError (AppGetArgsError, AppGetPasswordError, AppLoadConfigError))
import Args.Class (GetArgsError (GetArgsError), MonadArgs)
import qualified Args.Class as Args
import ConfigLoader.Class (MonadConfigLoader)
import qualified ConfigLoader.Class as ConfigLoader
import ConfigLoader.Config (Config (Config, user))
import Control.Monad.Error.Class (MonadError, liftEither)
import Control.Monad.Except (runExceptT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Bifunctor as Bifunctor
import qualified GetPassword
import LastPass.Class (MonadLastPass, Password)
import LastPass.Entry (Search)
import Printer.Class (MonadPrinter (printAppError, printPassword))
import System.Exit (exitFailure)

app :: (MonadArgs m, MonadIO m, MonadPrinter m, MonadConfigLoader m, MonadLastPass m) => m ()
app = do
  result <- runExceptT $ do
    search <- getSearch
    config <- loadConfig
    password <- getPassword config search
    printPassword password

  case result of
    Right _ -> return ()
    Left err -> printAppError err >> liftIO exitFailure

getSearch :: (MonadArgs m, MonadError AppError m) => m Search
getSearch = do
  Args.getSearch >>= wrapError (\GetArgsError {progName} -> AppGetArgsError progName)

loadConfig :: (MonadError AppError m, MonadConfigLoader m) => m Config
loadConfig = ConfigLoader.loadConfig >>= wrapError AppLoadConfigError

getPassword :: (MonadError AppError m, MonadLastPass m) => Config -> Search -> m Password
getPassword Config {user} search =
  runExceptT (GetPassword.getPassword user search) >>= wrapError AppGetPasswordError

wrapError :: MonadError e' m => (e -> e') -> Either e a -> m a
wrapError f = liftEither . Bifunctor.first f
