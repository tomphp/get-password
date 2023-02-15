{-# LANGUAGE OverloadedStrings #-}

module LastPass (MonadLastPass (..), LassPassT (..), LastPassError (..)) where

import Control.Monad.Error.Class (MonadError (catchError, throwError), liftEither)
import Control.Monad.Except (ExceptT, void)
import Control.Monad.Trans (MonadTrans (lift))
import Data.Bifunctor (first)
import Data.Text (pack, unpack)
import EntryListParser (parseEntryList)
import GHC.IO.Exception (ExitCode (ExitSuccess))
import PasswordEntry (PasswordEntry)
import System.Process.Text (readProcessWithExitCode)

data LastPassError
  = LastPassNotInstalled
  | LastPassNotLoggedIn
  | LastPassListPasswordsFailed
  | LastPassListPasswordsParseFailed String
  | LastPassShowPasswordFailed String
  | LastPassPasswordNotFound
  | LastPassMultiplePasswordsFound [PasswordEntry]
  deriving (Show, Eq)

class (MonadError LastPassError m, Monad m) => MonadLastPass m where
  checkIsInstalled :: m ()
  checkIsLoggedIn :: m ()
  listPasswords :: m [PasswordEntry]
  showPassword :: String -> m String

run :: FilePath -> [String] -> LastPassError -> LassPassT (ExceptT LastPassError IO) String
run cmd args err = do
  (exitCode, output, _) <-
    lift $
      lift $ readProcessWithExitCode cmd args ""
  case exitCode of
    ExitSuccess -> return $ unpack output
    _ -> liftEither (Left err)

lpass :: [String] -> LastPassError -> LassPassT (ExceptT LastPassError IO) String
lpass = run "lpass"

instance MonadLastPass (LassPassT (ExceptT LastPassError IO)) where
  checkIsInstalled =
    void $ lpass ["--version"] LastPassNotInstalled

  checkIsLoggedIn =
    void $ lpass ["status"] LastPassNotLoggedIn

  listPasswords = do
    output <- lpass ["ls", "--sync=now", "--format=%ai \"%an\" %al"] LastPassListPasswordsFailed
    liftEither $ first LastPassListPasswordsParseFailed (parseEntryList $ pack output)

  showPassword :: String -> (LassPassT (ExceptT LastPassError IO)) String
  showPassword entryId =
    run "lpass" ["show", "--password", entryId] (LastPassShowPasswordFailed "fixme")

newtype LassPassT m a = LassPassT {runLastPassT :: m a}
  deriving (Functor, Applicative, Monad)

instance MonadTrans LassPassT where
  lift = LassPassT

instance MonadError LastPassError m => MonadError LastPassError (LassPassT m) where
  throwError = lift . throwError
  catchError (LassPassT m) f = LassPassT $ catchError m (runLastPassT . f)
