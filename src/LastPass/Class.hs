module LastPass.Class (LastPassResult, MonadLastPass (..), User (User), Password (Password), LastPass (..), HasLastPass (..)) where

import Control.Monad.Except (ExceptT)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (MonadIO, ReaderT, ask)
import Control.Monad.Trans (MonadTrans (lift))
import Data.Text (Text)
import Data.Yaml (FromJSON)
import GHC.Generics (Generic)
import LastPass.Entry (Entry, EntryID)
import LastPass.Error (LastPassError)

type LastPassResult = Either LastPassError

newtype User = User Text
  deriving stock (Show, Eq, Generic)

instance FromJSON User

newtype Password = Password Text
  deriving stock (Show, Eq)

class Monad m => MonadLastPass m where
  checkIsInstalled :: m (LastPassResult ())
  isLoggedIn :: m Bool
  login :: User -> m (LastPassResult ())
  listPasswords :: m (LastPassResult [Entry])
  showPassword :: EntryID -> m (LastPassResult Password)

data LastPass = LastPass
  { checkIsInstalled_ :: IO (LastPassResult ()),
    isLoggedIn_ :: IO Bool,
    login_ :: User -> IO (LastPassResult ()),
    listPasswords_ :: IO (LastPassResult [Entry]),
    showPassword_ :: EntryID -> IO (LastPassResult Password)
  }

class HasLastPass env where
  getLastPass :: env -> LastPass

instance HasLastPass LastPass where
  getLastPass = id

instance (HasLastPass env, MonadIO m) => MonadLastPass (ReaderT env m) where
  checkIsInstalled = do
    env <- ask
    liftIO $ checkIsInstalled_ (getLastPass env)
  isLoggedIn = do
    env <- ask
    liftIO $ isLoggedIn_ (getLastPass env)
  login user = do
    env <- ask
    liftIO $ login_ (getLastPass env) user
  listPasswords = do
    env <- ask
    liftIO $ listPasswords_ (getLastPass env)
  showPassword entryID = do
    env <- ask
    liftIO $ showPassword_ (getLastPass env) entryID

instance MonadLastPass m => MonadLastPass (ExceptT e m) where
  checkIsInstalled = lift checkIsInstalled
  isLoggedIn = lift isLoggedIn
  login = lift . login
  listPasswords = lift listPasswords
  showPassword = lift . showPassword
