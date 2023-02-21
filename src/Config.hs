{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Config
  ( Config (..),
    ReadConfigError (..),
    LoadConfigError (..),
    defaultConfig,
    defaultIfDoesNotExist,
    getConfigPath,
    loadConfig,
    readConfig,
  )
where

import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Data.Bifunctor as Bifunctor
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Yaml (FromJSON (parseJSON), ParseException, withObject, (.!=), (.:?))
import qualified Data.Yaml as Yaml
import GHC.Generics (Generic)
import LastPass (User)
import qualified System.Directory as Dir
import System.FilePath ((</>))

newtype Config = Config
  { user :: Maybe User
  }
  deriving (Show, Eq, Generic)

instance FromJSON Config where
  parseJSON = withObject "Config" $ \obj ->
    Config
      <$> obj .:? "user" .!= Nothing

defaultConfig :: Config
defaultConfig = Config {user = Nothing}

newtype LoadConfigError = LoadConfigError Text
  deriving (Show, Eq)

loadConfig :: MonadIO m => m (Either LoadConfigError Config)
loadConfig = do
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
  deriving (Show, Eq)

readConfig :: MonadIO m => FilePath -> m (Either ReadConfigError Config)
readConfig path = do
  configExists <- liftIO $ Dir.doesFileExist path
  if configExists
    then Bifunctor.first (ConfigFileParseError . Text.pack . Yaml.prettyPrintParseException) <$> readAndParseFile path
    else return (Left ConfigFileDoesNotExist)

readAndParseFile :: MonadIO m => FilePath -> m (Either ParseException Config)
readAndParseFile = liftIO . Yaml.decodeFileEither
