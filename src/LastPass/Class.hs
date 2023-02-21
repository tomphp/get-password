module LastPass.Class (LastPassResult, MonadLastPass (..), User (User), Password (Password)) where

import Data.Text (Text)
import Data.Yaml (FromJSON)
import GHC.Generics (Generic)
import LastPass.Entry (Entry, EntryID)
import LastPass.Error (LastPassError)

type LastPassResult = Either LastPassError

newtype User = User Text
  deriving (Show, Eq, Generic)

instance FromJSON User

newtype Password = Password Text
  deriving (Show, Eq)

class Monad m => MonadLastPass m where
  checkIsInstalled :: m (LastPassResult ())
  isLoggedIn :: m Bool
  login :: User -> m (LastPassResult ())
  listPasswords :: m (LastPassResult [Entry])
  showPassword :: EntryID -> m (LastPassResult Password)
