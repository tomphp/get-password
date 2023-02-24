module ConfigLoader.Class where

import ConfigLoader.Config (Config)
import Lens.Micro.TH (makeClassy)
import RIO

newtype LoadConfigError = LoadConfigError Text
  deriving stock (Show, Eq)

newtype ConfigLoader = ConfigLoader
  { _loadConfig :: forall m. MonadIO m => m (Either LoadConfigError Config)
  }

makeClassy ''ConfigLoader

loadConfig_ :: (MonadReader env m, HasConfigLoader env, MonadIO m) => m (Either LoadConfigError Config)
loadConfig_ = ask >>= view loadConfig
