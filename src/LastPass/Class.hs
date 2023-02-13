module LastPass.Class (LastPassResult, MonadLastPass (..), User (User), Password (Password)) where

import Control.Monad.Except (ExceptT)
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

instance MonadLastPass m => MonadLastPass (ExceptT e m) where
  checkIsInstalled = lift checkIsInstalled
  isLoggedIn = lift isLoggedIn
  login = lift . login
  listPasswords = lift listPasswords
  showPassword = lift . showPassword
