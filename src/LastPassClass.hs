module LastPassClass (MonadLastPass (..)) where

import Data.Text (Text)
import Entry (Entry)
import LastPassError (LastPassError)

class Monad m => MonadLastPass m where
  checkIsInstalled :: m (Either LastPassError ())
  checkIsLoggedIn :: m (Either LastPassError ())
  listPasswords :: m (Either LastPassError [Entry])
  showPassword :: Text -> m (Either LastPassError Text)
