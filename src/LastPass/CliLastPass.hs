{-# LANGUAGE UndecidableInstances #-}

module LastPass.CliLastPass (CliLastPassT (..)) where

import Control.Monad.Error.Class (MonadError (catchError, throwError))
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans (MonadTrans (lift))
import LastPass.Class (MonadLastPass (..))
import qualified LastPass.Cli as Cli

newtype CliLastPassT m a = CliLastPassT {runCliLastPassT :: m a}
  deriving stock (Functor)
  deriving newtype (Applicative, Monad, MonadIO)

instance MonadTrans CliLastPassT where
  lift = CliLastPassT

instance MonadError e m => MonadError e (CliLastPassT m) where
  throwError = lift . throwError
  catchError (CliLastPassT m) f = CliLastPassT $ catchError m (runCliLastPassT . f)

instance MonadIO m => MonadLastPass (CliLastPassT m) where
  checkIsInstalled = Cli.checkIsInstalled
  isLoggedIn = Cli.isLoggedIn
  login = Cli.login
  listPasswords = Cli.listPasswords
  showPassword = Cli.showPassword