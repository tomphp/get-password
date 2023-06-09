module LastPass.Class
  ( LastPassResult,
    User (User),
    Password (Password),
    LastPass (..),
    HasLastPass (..),
    checkIsInstalled_,
    isLoggedIn_,
    login_,
    listPasswords_,
    showPassword_,
    copyPassword_,
  )
where

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

data LastPass = LastPass
  { _checkIsInstalled :: !(forall m. MonadIO m => m (LastPassResult ())),
    _isLoggedIn :: !(forall m. MonadIO m => m Bool),
    _login :: !(forall m. MonadIO m => User -> m (LastPassResult ())),
    _listPasswords :: !(forall m. MonadIO m => m (LastPassResult [Entry])),
    _showPassword :: !(forall m. MonadIO m => EntryID -> m (LastPassResult Password)),
    _copyPassword :: !(forall m. MonadIO m => EntryID -> m (LastPassResult ()))
  }

makeClassy ''LastPass

checkIsInstalled_ :: (MonadReader env m, HasLastPass env, MonadIO m) => m (LastPassResult ())
checkIsInstalled_ = ask >>= view checkIsInstalled

isLoggedIn_ :: (MonadReader env m, HasLastPass env, MonadIO m) => m Bool
isLoggedIn_ = ask >>= view isLoggedIn

login_ :: (MonadReader env m, HasLastPass env, MonadIO m) => User -> m (LastPassResult ())
login_ user = ask >>= view login <*> pure user

listPasswords_ :: (MonadReader env m, HasLastPass env, MonadIO m) => m (LastPassResult [Entry])
listPasswords_ = ask >>= view listPasswords

showPassword_ :: (MonadReader env m, HasLastPass env, MonadIO m) => EntryID -> m (LastPassResult Password)
showPassword_ entryID = ask >>= view showPassword <*> pure entryID

copyPassword_ :: (MonadReader env m, HasLastPass env, MonadIO m) => EntryID -> m (LastPassResult ())
copyPassword_ entryID = ask >>= view copyPassword <*> pure entryID
