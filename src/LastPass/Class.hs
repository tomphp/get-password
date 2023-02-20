module LastPass.Class (MonadLastPass (..)) where

import Data.Text (Text)
import LastPass.Entry (Entry)
import LastPass.Error (LastPassError)

class Monad m => MonadLastPass m where
  checkIsInstalled :: m (Either LastPassError ())
  isLoggedIn :: m Bool
  login :: Text -> m (Either LastPassError ())
  listPasswords :: m (Either LastPassError [Entry])
  showPassword :: Text -> m (Either LastPassError Text)
