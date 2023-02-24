module ConfigLoader.Class where

import ConfigLoader.Config (Config)
import Control.Monad.Except (ExceptT)
import Lens.Micro.TH (makeClassy)
import RIO

newtype LoadConfigError = LoadConfigError Text
  deriving stock (Show, Eq)

class Monad m => MonadConfigLoader m where
  loadConfig_ :: m (Either LoadConfigError Config)

newtype ConfigLoader = ConfigLoader
  { _loadConfig :: forall m. MonadIO m => m (Either LoadConfigError Config)
  }

makeClassy ''ConfigLoader

instance (HasConfigLoader env, MonadIO m) => MonadConfigLoader (ReaderT env m) where
  loadConfig_ = do
    env <- ask
    liftIO $ _loadConfig (env ^. configLoader)

instance (MonadConfigLoader m) => MonadConfigLoader (ExceptT e m) where
  loadConfig_ = lift loadConfig_
