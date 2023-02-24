module Args.Class (Args (..), HasArgs (..), MonadArgs (..), GetArgsError (..)) where

import Control.Monad.Except (ExceptT)
import LastPass.Entry (Search)
import Lens.Micro.TH (makeClassy)
import RIO
import System.Class (HasSystem, MonadSystem)

newtype GetArgsError = GetArgsError {progName :: Text}
  deriving stock (Show, Eq)

class Monad m => MonadArgs m where
  getSearch_ :: m (Either GetArgsError Search)

newtype Args = Args
  { _getSearch :: forall m. MonadSystem m => m (Either GetArgsError Search)
  }

makeClassy ''Args

instance (HasArgs env, HasSystem env, MonadIO m) => MonadArgs (ReaderT env m) where
  getSearch_ = do
    env <- ask
    env ^. getSearch

instance (MonadArgs m) => MonadArgs (ExceptT e m) where
  getSearch_ = lift getSearch_
