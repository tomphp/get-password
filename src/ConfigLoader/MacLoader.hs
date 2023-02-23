module ConfigLoader.MacLoader
  ( ReadConfigError (..),
    defaultIfDoesNotExist,
    getConfigPath,
    loadConfig,
    readConfig,
    macConfigLoader,
  )
where

import ConfigLoader.Class (ConfigLoader (ConfigLoader, loadConfig_), LoadConfigError (LoadConfigError))
import ConfigLoader.Config (Config (..), defaultConfig)
import Data.Yaml (ParseException)
import qualified Data.Yaml as Yaml
import RIO
import qualified RIO.Text as Text
import qualified System.Directory as Dir
import System.FilePath ((</>))

macConfigLoader :: ConfigLoader
macConfigLoader =
  ConfigLoader
    { loadConfig_ = loadConfig
    }

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
  deriving stock (Show, Eq)

readConfig :: MonadIO m => FilePath -> m (Either ReadConfigError Config)
readConfig path = do
  configExists <- liftIO $ Dir.doesFileExist path
  if configExists
    then first (ConfigFileParseError . Text.pack . Yaml.prettyPrintParseException) <$> readAndParseFile path
    else return (Left ConfigFileDoesNotExist)

readAndParseFile :: MonadIO m => FilePath -> m (Either ParseException Config)
readAndParseFile = liftIO . Yaml.decodeFileEither
