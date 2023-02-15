module LastPass (MonadLastPass (..), LassPassT (..), LastPassError (..)) where

import Control.Monad.Error.Class (MonadError (catchError, throwError), liftEither)
import Control.Monad.Except (ExceptT, void)
import Control.Monad.Trans (MonadTrans (lift))
import Data.Bifunctor (first)
import Data.Text (Text, unpack)
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
  showPassword :: Text -> m Text

run :: FilePath -> [String] -> LastPassError -> LassPassT (ExceptT LastPassError IO) Text
run cmd args err = do
  (exitCode, output, _) <-
    lift $
      lift $ readProcessWithExitCode cmd args ""
  case exitCode of
    ExitSuccess -> return output
    _ -> liftEither (Left err)

lpass :: [String] -> LastPassError -> LassPassT (ExceptT LastPassError IO) Text
lpass = run "lpass"

instance MonadLastPass (LassPassT (ExceptT LastPassError IO)) where
  checkIsInstalled =
    void $ lpass ["--version"] LastPassNotInstalled

  checkIsLoggedIn =
    void $ lpass ["status"] LastPassNotLoggedIn

  listPasswords = do
    output <- lpass ["ls", "--sync=now", "--format=%ai \"%an\" %al"] LastPassListPasswordsFailed
    liftEither $ first LastPassListPasswordsParseFailed (parseEntryList output)

  showPassword entryId =
    run "lpass" ["show", "--password", unpack entryId] (LastPassShowPasswordFailed "fixme")

newtype LassPassT m a = LassPassT {runLastPassT :: m a}
  deriving (Functor, Applicative, Monad)

instance MonadTrans LassPassT where
  lift = LassPassT

instance MonadError LastPassError m => MonadError LastPassError (LassPassT m) where
  throwError = lift . throwError
  catchError (LassPassT m) f = LassPassT $ catchError m (runLastPassT . f)
