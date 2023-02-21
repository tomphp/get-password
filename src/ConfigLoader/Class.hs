module ConfigLoader.Class where

import ConfigLoader.Config (Config)
import Data.Text (Text)

newtype LoadConfigError = LoadConfigError Text
  deriving stock (Show, Eq)

class MonadConfigLoader m where
  loadConfig :: m (Either LoadConfigError Config)
