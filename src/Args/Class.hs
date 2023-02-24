module Args.Class (Args (..), HasArgs (..), MonadArgs (..), GetArgsError (..)) where

import Control.Monad.Except (ExceptT)
import LastPass.Entry (Search)
import RIO

newtype GetArgsError = GetArgsError {progName :: Text}

class Monad m => MonadArgs m where
  getSearch :: m (Either GetArgsError Search)

newtype Args = Args
  { getSearch_ :: forall m. MonadIO m => m (Either GetArgsError Search)
  }

class HasArgs env where
  getArgs :: env -> Args

instance HasArgs Args where
  getArgs = id

instance (HasArgs env, MonadIO m) => MonadArgs (ReaderT env m) where
  getSearch = do
    env <- ask
    liftIO $ getSearch_ (getArgs env)

instance (MonadArgs m) => MonadArgs (ExceptT e m) where
  getSearch = lift getSearch
