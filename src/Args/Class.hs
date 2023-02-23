module Args.Class (Args (..), HasArgs (..), MonadArgs (..), GetArgsError (..)) where

import Control.Monad.Except (ExceptT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (ReaderT, ask)
import Control.Monad.Trans (lift)
import Data.Text (Text)
import LastPass.Entry (Search)

newtype GetArgsError = GetArgsError {progName :: Text}

class Monad m => MonadArgs m where
  getSearch :: m (Either GetArgsError Search)

newtype Args = Args
  { getSearch_ :: IO (Either GetArgsError Search)
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
