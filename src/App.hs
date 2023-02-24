module App (app, AppError (..)) where

import App.Error (AppError (AppGetArgsError, AppGetPasswordError, AppLoadConfigError))
import Args.Class (GetArgsError (GetArgsError), HasArgs)
import qualified Args.Class as Args
import ConfigLoader.Class (HasConfigLoader)
import qualified ConfigLoader.Class as ConfigLoader
import ConfigLoader.Config (Config (Config, user))
import Control.Monad.Error.Class (MonadError, liftEither)
import Control.Monad.Except (runExceptT)
import qualified GetPassword
import LastPass.Class (HasLastPass, Password)
import LastPass.Entry (Search)
import Printer.Class (HasPrinter, printAppError_, printPassword_)
import RIO
import System.Class (HasSystem)

app :: (MonadIO m, MonadReader env m, HasArgs env, HasPrinter env, HasConfigLoader env, HasLastPass env, HasSystem env) => m ()
app = runExceptT lookupPassword >>= printResult

lookupPassword :: (MonadIO m, MonadReader env m, HasArgs env, MonadError AppError m, HasConfigLoader env, HasLastPass env, HasSystem env) => m Password
lookupPassword = do
  search <- getSearch
  config <- loadConfig
  getPassword config search

printResult :: (MonadReader env m, HasPrinter env, MonadIO m) => Either AppError Password -> m ()
printResult (Right password) = printPassword_ password
printResult (Left err) = printAppError_ err >> liftIO exitFailure

getSearch :: (MonadIO m, MonadReader env m, HasArgs env, MonadError AppError m, HasSystem env) => m Search
getSearch =
  Args.getSearch_ >>= wrapError (\GetArgsError {progName} -> AppGetArgsError progName)

loadConfig :: (MonadReader env m, MonadError AppError m, HasConfigLoader env, MonadIO m) => m Config
loadConfig = ConfigLoader.loadConfig_ >>= wrapError AppLoadConfigError

getPassword :: (MonadIO m, MonadError AppError m, HasLastPass env, MonadReader env m) => Config -> Search -> m Password
getPassword Config {user} search =
  runExceptT (GetPassword.getPassword user search) >>= wrapError AppGetPasswordError

wrapError :: MonadError e' m => (e -> e') -> Either e a -> m a
wrapError f = liftEither . first f
