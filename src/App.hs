module App (app, AppError (..)) where

import App.Error (AppError (AppGetArgsError, AppGetPasswordError, AppLoadConfigError))
import Args.Class (GetArgsError (GetArgsError), HasArgs)
import qualified Args.Class as Args
import ConfigLoader.Class (HasConfigLoader)
import qualified ConfigLoader.Class as ConfigLoader
import ConfigLoader.Config (Config (Config, user))
import Control.Monad.Error.Class (MonadError, liftEither)
import Control.Monad.Except (runExceptT)
import GetPassword (GetAction, GetPasswordResult (CopiedPassword, FetchedPassword))
import qualified GetPassword
import LastPass.Class (HasLastPass)
import LastPass.Entry (Search)
import Printer.Class (HasPrinter, printAppError_, printCopiedMessage_, printPassword_)
import RIO
import System.Class (HasSystem)

app :: (MonadIO m, MonadReader env m, HasArgs env, HasPrinter env, HasConfigLoader env, HasLastPass env, HasSystem env) => m ()
app = runExceptT lookupPassword >>= printResult

lookupPassword :: (MonadIO m, MonadReader env m, HasArgs env, MonadError AppError m, HasConfigLoader env, HasLastPass env, HasSystem env) => m GetPasswordResult
lookupPassword = do
  search <- getSearch
  action <- getAction
  config <- loadConfig
  getPassword config action search

printResult :: (MonadReader env m, HasPrinter env, MonadIO m) => Either AppError GetPasswordResult -> m ()
printResult (Right (FetchedPassword password)) = printPassword_ password
printResult (Right CopiedPassword) = printCopiedMessage_
printResult (Left err) = printAppError_ err >> liftIO exitFailure

getSearch :: (MonadIO m, MonadReader env m, HasArgs env, MonadError AppError m, HasSystem env) => m Search
getSearch =
  Args.getSearch_ >>= wrapError (\GetArgsError {progName} -> AppGetArgsError progName)

getAction :: (MonadIO m, MonadReader env m, HasArgs env, MonadError AppError m, HasSystem env) => m GetAction
getAction =
  Args.getAction_ >>= wrapError (\GetArgsError {progName} -> AppGetArgsError progName)

loadConfig :: (MonadReader env m, MonadError AppError m, HasConfigLoader env, MonadIO m) => m Config
loadConfig = ConfigLoader.loadConfig_ >>= wrapError AppLoadConfigError

getPassword :: (MonadIO m, MonadError AppError m, HasLastPass env, MonadReader env m) => Config -> GetAction -> Search -> m GetPasswordResult
getPassword Config {user} action search =
  runExceptT (GetPassword.getPassword user action search) >>= wrapError AppGetPasswordError

wrapError :: MonadError e' m => (e -> e') -> Either e a -> m a
wrapError f = liftEither . first f
