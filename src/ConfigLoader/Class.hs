module ConfigLoader.Class where

import ConfigLoader.Config (Config)
import Control.Monad.Except (ExceptT)
import RIO

newtype LoadConfigError = LoadConfigError Text
  deriving stock (Show, Eq)

class Monad m => MonadConfigLoader m where
  loadConfig :: m (Either LoadConfigError Config)

newtype ConfigLoader = ConfigLoader
  { loadConfig_ :: IO (Either LoadConfigError Config)
  }

class HasConfigLoader env where
  getConfigLoader :: env -> ConfigLoader

instance HasConfigLoader ConfigLoader where
  getConfigLoader = id

instance (HasConfigLoader env, MonadIO m) => MonadConfigLoader (ReaderT env m) where
  loadConfig = do
    env <- ask
    liftIO $ loadConfig_ (getConfigLoader env)

instance (Monad m, MonadConfigLoader m) => MonadConfigLoader (ExceptT e m) where
  loadConfig = lift loadConfig
