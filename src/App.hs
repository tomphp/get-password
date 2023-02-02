module App (app, AppError (..)) where

import App.Error (AppError (AppGetArgsError, AppGetPasswordError, AppLoadConfigError))
import ConfigLoader.Class (MonadConfigLoader)
import qualified ConfigLoader.Class as ConfigLoader
import ConfigLoader.Config (Config (Config, user))
import Control.Monad.Error.Class (MonadError (throwError), liftEither)
import Control.Monad.Except (runExceptT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Bifunctor as Bifunctor
import Data.Text (Text)
import qualified Data.Text as Text
import qualified GetPassword
import LastPass.Class (MonadLastPass, Password)
import LastPass.Entry (Search (Search))
import Printer.Class (MonadPrinter (printAppError, printPassword))
import qualified System.Environment as Env
import System.Exit (exitFailure)

app :: (MonadIO m, MonadPrinter m, MonadConfigLoader m, MonadLastPass m) => m ()
app = do
  result <- runExceptT $ do
    search <- getArgs
    config <- loadConfig
    password <- getPassword config search
    printPassword password

  case result of
    Right _ -> return ()
    Left err -> printAppError err >> liftIO exitFailure

getArgs :: (MonadError AppError m, MonadIO m) => m Search
getArgs = do
  args <- liftIO Env.getArgs
  maybe throwGetArgsError return (parseArgs args)
  where
    parseArgs [search] = Just $ Search $ Text.pack search
    parseArgs _ = Nothing

    throwGetArgsError = throwError . AppGetArgsError =<< getProgName

getProgName :: MonadIO m => m Text
getProgName = Text.pack <$> liftIO Env.getProgName

loadConfig :: (MonadError AppError m, MonadConfigLoader m) => m Config
loadConfig = ConfigLoader.loadConfig >>= wrapError AppLoadConfigError

getPassword :: (MonadError AppError m, MonadLastPass m) => Config -> Search -> m Password
getPassword Config {user} search =
  runExceptT (GetPassword.getPassword user search) >>= wrapError AppGetPasswordError

wrapError :: MonadError e' m => (e -> e') -> Either e a -> m a
wrapError f = liftEither . Bifunctor.first f
