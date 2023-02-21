module Config.MacLoader
  ( MacLoaderT (runMacLoaderT),
    ReadConfigError (..),
    LoadConfigError (..),
    defaultIfDoesNotExist,
    getConfigPath,
    loadConfig,
    readConfig,
  )
where

import Config.Config (Config (..), defaultConfig)
import Config.Loader (ConfigLoaderMonad (loadConfig), LoadConfigError (LoadConfigError))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans (MonadTrans, lift)
import qualified Data.Bifunctor as Bifunctor
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Yaml (ParseException)
import qualified Data.Yaml as Yaml
import Printer.Class (MonadPrinter (printError, printLoadConfigError, printPassword, printUsage))
import qualified System.Directory as Dir
import System.FilePath ((</>))

newtype MacLoaderT m a = MacLoaderT {runMacLoaderT :: m a}
  deriving stock (Functor)
  deriving newtype (Applicative, Monad, MonadIO)

instance MonadTrans MacLoaderT where
  lift = MacLoaderT

instance (Monad m, MonadPrinter m) => MonadPrinter (MacLoaderT m) where
  printPassword = lift . printPassword
  printUsage = lift printUsage
  printError = lift . printError
  printLoadConfigError = lift . printLoadConfigError

instance MonadIO m => ConfigLoaderMonad (MacLoaderT m) where
  loadConfig = liftIO loadConfig'

loadConfig' :: MonadIO m => m (Either LoadConfigError Config)
loadConfig' = do
  path <- getConfigPath
  config <- readConfig path
  return $ defaultIfDoesNotExist defaultConfig config

getConfigPath :: MonadIO m => m FilePath
getConfigPath = do
  home <- liftIO Dir.getHomeDirectory
  return $ home </> ".config" </> "get-password" </> "config.yml"

defaultIfDoesNotExist :: Config -> Either ReadConfigError Config -> Either LoadConfigError Config
defaultIfDoesNotExist _ (Right config) = Right config
defaultIfDoesNotExist defaultConfig' (Left ConfigFileDoesNotExist) = Right defaultConfig'
defaultIfDoesNotExist _ (Left (ConfigFileParseError err)) = Left (LoadConfigError err)

data ReadConfigError
  = ConfigFileDoesNotExist
  | ConfigFileParseError !Text
  deriving stock (Show, Eq)

readConfig :: MonadIO m => FilePath -> m (Either ReadConfigError Config)
readConfig path = do
  configExists <- liftIO $ Dir.doesFileExist path
  if configExists
    then Bifunctor.first (ConfigFileParseError . Text.pack . Yaml.prettyPrintParseException) <$> readAndParseFile path
    else return (Left ConfigFileDoesNotExist)

readAndParseFile :: MonadIO m => FilePath -> m (Either ParseException Config)
readAndParseFile = liftIO . Yaml.decodeFileEither
