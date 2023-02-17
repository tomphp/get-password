module LastPass (MonadLastPass (..), LassPassT (..)) where

import Control.Monad.Error.Class (MonadError (catchError, throwError), liftEither)
import Control.Monad.Except (ExceptT)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans (MonadTrans (lift))
import Data.Text (Text)
import Entry (Entry)
import qualified LastPassCli
import LastPassError (LastPassError)

class Monad m => MonadLastPass m where
  checkIsInstalled :: m ()
  checkIsLoggedIn :: m ()
  listPasswords :: m [Entry]
  showPassword :: Text -> m Text

instance MonadLastPass (LassPassT (ExceptT LastPassError IO)) where
  checkIsInstalled = liftIO LastPassCli.checkIsInstalled >>= liftEither

  checkIsLoggedIn = liftIO LastPassCli.checkIsLoggedIn >>= liftEither

  listPasswords = liftIO LastPassCli.listPasswords >>= liftEither

  showPassword entryId = liftIO (LastPassCli.showPassword entryId) >>= liftEither

newtype LassPassT m a = LassPassT {runLastPassT :: m a}
  deriving (Functor, Applicative, Monad, MonadIO)

instance MonadTrans LassPassT where
  lift = LassPassT

instance MonadError LastPassError m => MonadError LastPassError (LassPassT m) where
  throwError = lift . throwError
  catchError (LassPassT m) f = LassPassT $ catchError m (runLastPassT . f)
