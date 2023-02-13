module ConfigLoader.Class where

import ConfigLoader.Config (Config)
import Control.Monad.Except (ExceptT)
import Control.Monad.State.Lazy (lift)
import Data.Text (Text)

newtype LoadConfigError = LoadConfigError Text
  deriving stock (Show, Eq)

class Monad m => MonadConfigLoader m where
  loadConfig :: m (Either LoadConfigError Config)

instance (Monad m, MonadConfigLoader m) => MonadConfigLoader (ExceptT e m) where
  loadConfig = lift loadConfig
