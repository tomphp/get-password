module App (app) where

import qualified Args
import ConfigLoader.Class (LoadConfigError, MonadConfigLoader)
import qualified ConfigLoader.Class as ConfigLoader
import ConfigLoader.Config (Config (Config, user))
import Console.Class (MonadConsole)
import Control.Monad.Error.Class (liftEither)
import Control.Monad.Except (MonadError, runExceptT)
import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Data.Bifunctor as Bifunctor
import Data.Either.Extra (maybeToEither)
import Data.Text (Text)
import qualified Data.Text as Text
import GetPassword (GetPasswordError (..))
import qualified GetPassword
import LastPass.Class (MonadLastPass, Password)
import LastPass.Entry (Search)
import Printer.Class (MonadPrinter (printGetPasswordError, printLoadConfigError, printPassword, printUsage))
import qualified System.Environment as Env
import System.Exit (exitFailure)

data AppError
  = AppGetArgsError
  | AppLoadConfigError LoadConfigError
  | AppGetPasswordError GetPasswordError

app :: (MonadIO m, MonadPrinter m, MonadConsole m, MonadConfigLoader m, MonadLastPass m) => m ()
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
getArgs = Args.getArgs >>= wrapError (const AppGetArgsError) . maybeToEither ()

loadConfig :: (MonadError AppError m, MonadConfigLoader m) => m Config
loadConfig = ConfigLoader.loadConfig >>= wrapError AppLoadConfigError

getPassword :: (MonadError AppError m, MonadLastPass m) => Config -> Search -> m Password
getPassword Config {user} search =
  runExceptT (GetPassword.getPassword user search) >>= wrapError AppGetPasswordError

printError :: (MonadPrinter m, MonadConsole m, MonadIO m) => AppError -> m ()
printError AppGetArgsError = getProgName >>= printUsage
printError (AppLoadConfigError err) = printLoadConfigError err
printError (AppGetPasswordError err) = printGetPasswordError err

getProgName :: MonadIO m => m Text
getProgName = Text.pack <$> liftIO Env.getProgName

wrapError :: (MonadError e' m) => (e -> e') -> Either e a -> m a
wrapError f = liftEither . Bifunctor.first f
