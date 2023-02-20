module LastPass.Class (MonadLastPass (..)) where

import Data.Text (Text)
import LastPass.Cli (LastPassResult)
import LastPass.Entry (Entry)

class Monad m => MonadLastPass m where
  checkIsInstalled :: m (LastPassResult ())
  isLoggedIn :: m Bool
  login :: Text -> m (LastPassResult ())
  listPasswords :: m (LastPassResult [Entry])
  showPassword :: Text -> m (LastPassResult Text)
