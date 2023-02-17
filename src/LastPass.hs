{-# LANGUAGE UndecidableInstances #-}

module LastPass (LastPassT (..)) where

import Control.Monad.Error.Class (MonadError (catchError, throwError))
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans (MonadTrans (lift))
import LastPassClass (MonadLastPass (..))
import qualified LastPassCli

newtype LastPassT m a = LastPassT {runLastPassT :: m a}
  deriving (Functor, Applicative, Monad, MonadIO)

instance MonadTrans LastPassT where
  lift = LastPassT

instance MonadError e m => MonadError e (LastPassT m) where
  throwError = lift . throwError
  catchError (LastPassT m) f = LastPassT $ catchError m (runLastPassT . f)

instance MonadIO m => MonadLastPass (LastPassT m) where
  checkIsInstalled = LastPassCli.checkIsInstalled
  checkIsLoggedIn = LastPassCli.checkIsLoggedIn
  listPasswords = LastPassCli.listPasswords
  showPassword = LastPassCli.showPassword
