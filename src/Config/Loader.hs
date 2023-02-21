module Config.Loader where

import Config.Config (Config)
import Data.Text (Text)

newtype LoadConfigError = LoadConfigError Text
  deriving (Show, Eq)

class ConfigLoaderMonad m where
  loadConfig :: m (Either LoadConfigError Config)
