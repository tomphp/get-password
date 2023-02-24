module LastPass.Class (LastPassResult, MonadLastPass (..), User (User), Password (Password), LastPass (..), HasLastPass (..)) where

import Control.Monad.Except (ExceptT)
import Data.Yaml (FromJSON)
import LastPass.Entry (Entry, EntryID)
import LastPass.Error (LastPassError)
import Lens.Micro.TH (makeClassy)
import RIO

type LastPassResult = Either LastPassError

newtype User = User Text
  deriving stock (Show, Eq, Generic)

instance FromJSON User

newtype Password = Password Text
  deriving stock (Show, Eq)

class Monad m => MonadLastPass m where
  checkIsInstalled_ :: m (LastPassResult ())
  isLoggedIn_ :: m Bool
  login_ :: User -> m (LastPassResult ())
  listPasswords_ :: m (LastPassResult [Entry])
  showPassword_ :: EntryID -> m (LastPassResult Password)

data LastPass = LastPass
  { _checkIsInstalled :: !(forall m. MonadIO m => m (LastPassResult ())),
    _isLoggedIn :: !(forall m. MonadIO m => m Bool),
    _login :: !(forall m. MonadIO m => User -> m (LastPassResult ())),
    _listPasswords :: !(forall m. MonadIO m => m (LastPassResult [Entry])),
    _showPassword :: !(forall m. MonadIO m => EntryID -> m (LastPassResult Password))
  }

makeClassy ''LastPass

instance (HasLastPass env, MonadIO m) => MonadLastPass (ReaderT env m) where
  checkIsInstalled_ = ask >>= view checkIsInstalled
  isLoggedIn_ = ask >>= view isLoggedIn
  login_ user = ask >>= view login <*> pure user
  listPasswords_ = ask >>= view listPasswords
  showPassword_ entryID = ask >>= view showPassword <*> pure entryID

instance MonadLastPass m => MonadLastPass (ExceptT e m) where
  checkIsInstalled_ = lift checkIsInstalled_
  isLoggedIn_ = lift isLoggedIn_
  login_ = lift . login_
  listPasswords_ = lift listPasswords_
  showPassword_ = lift . showPassword_
