module App (app) where

import ConfigLoader.Class (LoadConfigError, MonadConfigLoader)
import qualified ConfigLoader.Class as ConfigLoader
import ConfigLoader.Config (Config (Config, user))
import Control.Monad.Error.Class (MonadError (throwError), liftEither)
import Control.Monad.Except (runExceptT)
import Control.Monad.IO.Class (MonadIO (liftIO), liftIO)
import qualified Data.Bifunctor as Bifunctor
import Data.Text (Text)
import qualified Data.Text as Text
import GetPassword (GetPasswordError (..))
import qualified GetPassword
import LastPass.Class (MonadLastPass, Password)
import LastPass.Entry (Search (Search))
import Printer.Class (MonadPrinter (printGetPasswordError, printLoadConfigError, printPassword, printUsage))
import qualified System.Environment as Env
import System.Exit (exitFailure)

data AppError
  = AppGetArgsError Text
  | AppLoadConfigError LoadConfigError
  | AppGetPasswordError GetPasswordError

app :: (MonadIO m, MonadPrinter m, MonadConfigLoader m, MonadLastPass m) => m ()
app = do
  result <- runExceptT $ do
    search <- getArgs
    config <- loadConfig
    password <- getPassword config search
    printPassword password

  case result of
    Right _ -> return ()
    Left err -> printError err >> liftIO exitFailure

getArgs :: (MonadError AppError m, MonadIO m) => m Search
getArgs = do
  args <- liftIO Env.getArgs
  maybe throwGetArgsError return (parseArgs args)
  where
    parseArgs [search] = Just $ Search $ Text.pack search
    parseArgs _ = Nothing

    throwGetArgsError = throwError . AppGetArgsError =<< getProgName

loadConfig :: (MonadError AppError m, MonadConfigLoader m) => m Config
loadConfig = ConfigLoader.loadConfig >>= wrapError AppLoadConfigError

getPassword :: (MonadError AppError m, MonadLastPass m) => Config -> Search -> m Password
getPassword Config {user} search =
  runExceptT (GetPassword.getPassword user search) >>= wrapError AppGetPasswordError

printError :: MonadPrinter m => AppError -> m ()
printError (AppGetArgsError progName) = printUsage progName
printError (AppLoadConfigError err) = printLoadConfigError err
printError (AppGetPasswordError err) = printGetPasswordError err

getProgName :: MonadIO m => m Text
getProgName = Text.pack <$> liftIO Env.getProgName

wrapError :: MonadError e' m => (e -> e') -> Either e a -> m a
wrapError f = liftEither . Bifunctor.first f
