module Args.Class (Args (..), HasArgs (..), getSearch_, GetArgsError (..)) where

import LastPass.Entry (Search)
import Lens.Micro.TH (makeClassy)
import RIO
import System.Class (HasSystem)

newtype GetArgsError = GetArgsError {progName :: Text}
  deriving stock (Show, Eq)

newtype Args = Args
  { _getSearch :: forall m. forall env. (MonadReader env m, HasSystem env, MonadIO m) => m (Either GetArgsError Search)
  }

makeClassy ''Args

getSearch_ :: (MonadIO m, MonadReader env m, HasArgs env, HasSystem env) => m (Either GetArgsError Search)
getSearch_ = ask >>= view getSearch
