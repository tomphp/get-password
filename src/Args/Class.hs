module Args.Class (Args (..), HasArgs (..), getSearch_, getAction_, GetArgsError (..)) where

import GetPassword (GetAction)
import LastPass.Entry (Search)
import Lens.Micro.TH (makeClassy)
import RIO
import System.Class (HasSystem)

newtype GetArgsError = GetArgsError {progName :: Text}
  deriving stock (Show, Eq)

data Args = Args
  { _getSearch :: forall m. forall env. (MonadReader env m, HasSystem env, MonadIO m) => m (Either GetArgsError Search),
    _getAction :: forall m. forall env. (MonadReader env m, HasSystem env, MonadIO m) => m (Either GetArgsError GetAction)
  }

makeClassy ''Args

getSearch_ :: (MonadIO m, MonadReader env m, HasArgs env, HasSystem env) => m (Either GetArgsError Search)
getSearch_ = ask >>= view getSearch

getAction_ :: (MonadIO m, MonadReader env m, HasArgs env, HasSystem env) => m (Either GetArgsError GetAction)
getAction_ = ask >>= view getAction
