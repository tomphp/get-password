module LastPass (MonadLastPass (..), LassPassT (..), LastPassError (..)) where

import Control.Monad (void)
import Control.Monad.Error.Class (MonadError (catchError, throwError), liftEither)
import Control.Monad.Except (ExceptT)
import Control.Monad.Trans (MonadTrans (lift))
import Data.Bifunctor (first)
import Data.Text (Text)
import qualified Data.Text as Text
import Entry (Entry)
import EntryListParser (parseEntryList)
import GHC.IO.Exception (ExitCode (ExitSuccess))
import System.Process.Text (readProcessWithExitCode)

data LastPassError
  = LastPassNotInstalled
  | LastPassNotLoggedIn
  | LastPassListPasswordsFailed
  | LastPassListPasswordsParseFailed String
  | LastPassShowPasswordFailed String
  | LastPassPasswordNotFound
  | LastPassMultiplePasswordsFound [Entry]
  deriving (Show, Eq)

class (MonadError LastPassError m, Monad m) => MonadLastPass m where
  checkIsInstalled :: m ()
  checkIsLoggedIn :: m ()
  listPasswords :: m [Entry]
  showPassword :: Text -> m Text

run :: FilePath -> [String] -> e -> IO (Either e Text)
run cmd args err = do
  (exitCode, output, _) <- readProcessWithExitCode cmd args ""
  case exitCode of
    ExitSuccess -> return (Right output)
    _ -> return (Left err)

lpass' :: [String] -> LastPassError -> IO (Either LastPassError Text)
lpass' = run "lpass"

lpass :: [String] -> LastPassError -> LassPassT (ExceptT LastPassError IO) Text
lpass args err = do
  result <- lift $ lift $ lpass' args err
  case result of
    Left e -> throwError e
    Right output -> return output

instance MonadLastPass (LassPassT (ExceptT LastPassError IO)) where
  checkIsInstalled =
    void $ lpass ["--version"] LastPassNotInstalled

  checkIsLoggedIn =
    void $ lpass ["status"] LastPassNotLoggedIn

  listPasswords = do
    output <- lpass ["ls", "--sync=now", "--format=%ai \"%an\" %al"] LastPassListPasswordsFailed
    liftEither $ first LastPassListPasswordsParseFailed (parseEntryList output)

  showPassword entryId =
    lpass ["show", "--password", Text.unpack entryId] (LastPassShowPasswordFailed "fixme")

newtype LassPassT m a = LassPassT {runLastPassT :: m a}
  deriving (Functor, Applicative, Monad)

instance MonadTrans LassPassT where
  lift = LassPassT

instance MonadError LastPassError m => MonadError LastPassError (LassPassT m) where
  throwError = lift . throwError
  catchError (LassPassT m) f = LassPassT $ catchError m (runLastPassT . f)
