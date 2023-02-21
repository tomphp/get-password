module LastPassMock
  ( run,
    listPasswordsWillReturn,
    showPasswordWillReturn,
    checkIsInstalledWillErrorWith,
    isLoggedInWillReturn,
    listPasswordsWillErrorWith,
    loginWillErrorWith,
    showPasswordWillErrorWith,
  )
where

import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Identity (Identity (runIdentity))
import Control.Monad.RWS (MonadRWS, MonadState, MonadTrans, RWST (runRWST), lift)
import Control.Monad.Reader.Class (MonadReader)
import Control.Monad.State (gets, modify)
import Control.Monad.Writer (MonadWriter, tell)
import Data.Text (Text)
import LastPass (Entry (Entry), EntryID (EntryID), LastPassError, LastPassResult, MonadLastPass (..), User (User))

type Command = Text

data Results = Results
  { checkIsInstalledResult :: LastPassResult (),
    isLoggedInResult :: Bool,
    loginResult :: LastPassResult (),
    listPasswordsResult :: LastPassResult [Entry],
    showPasswordResult :: LastPassResult Text
  }

type MockLastPass = MockLastPassT Identity

newtype MockLastPassT m a = MockLastPass
  { runMockLastPassT' :: RWST () [Command] Results m a
  }
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadWriter [Command],
      MonadReader (),
      MonadState Results,
      MonadRWS () [Command] Results,
      MonadTrans
    )

runMockLastPassT :: Monad m => MockLastPassT m a -> m (a, [Command])
runMockLastPassT mock = do
  (result, _, history) <- runRWST (runMockLastPassT' mock) () defaultResults
  return (result, history)

runMockLastPass :: MockLastPass a -> (a, [Command])
runMockLastPass = runIdentity . runMockLastPassT

defaultResults :: Results
defaultResults =
  Results
    { checkIsInstalledResult = Right (),
      isLoggedInResult = True,
      loginResult = Right (),
      listPasswordsResult = Right [Entry (EntryID "default-id") "default-name" "default-url"],
      showPasswordResult = Right ""
    }

mockResult :: Monad m => Text -> (Results -> a) -> MockLastPassT m a
mockResult command getter = do
  tell [command]
  gets getter

instance Monad m => MonadLastPass (MockLastPassT m) where
  checkIsInstalled = mockResult "checkIsInstalled" checkIsInstalledResult
  isLoggedIn = mockResult "isLoggedIn" isLoggedInResult
  login (User user) = mockResult ("login \"" <> user <> "\"") loginResult
  listPasswords = mockResult "listPasswords" listPasswordsResult
  showPassword (EntryID entryID) = mockResult ("showPassword \"" <> entryID <> "\"") showPasswordResult

listPasswordsWillReturn :: Monad m => [Entry] -> MockLastPassT m ()
listPasswordsWillReturn value =
  modify $ \state -> state {listPasswordsResult = Right value}

isLoggedInWillReturn :: Monad m => Bool -> MockLastPassT m ()
isLoggedInWillReturn value =
  modify $ \state -> state {isLoggedInResult = value}

showPasswordWillReturn :: Monad m => Text -> MockLastPassT m ()
showPasswordWillReturn value =
  modify $ \state -> state {showPasswordResult = Right value}

checkIsInstalledWillErrorWith :: Monad m => LastPassError -> MockLastPassT m ()
checkIsInstalledWillErrorWith err =
  modify $ \state -> state {checkIsInstalledResult = Left err}

loginWillErrorWith :: Monad m => LastPassError -> MockLastPassT m ()
loginWillErrorWith err =
  modify $ \state -> state {loginResult = Left err}

listPasswordsWillErrorWith :: Monad m => LastPassError -> MockLastPassT m ()
listPasswordsWillErrorWith err =
  modify $ \state -> state {listPasswordsResult = Left err}

showPasswordWillErrorWith :: Monad m => LastPassError -> MockLastPassT m ()
showPasswordWillErrorWith err =
  modify $ \state -> state {showPasswordResult = Left err}

instance MonadLastPass (ExceptT e (MockLastPassT Identity)) where
  checkIsInstalled = lift checkIsInstalled
  isLoggedIn = lift isLoggedIn
  login = lift . login
  listPasswords = lift listPasswords
  showPassword search = lift $ showPassword search

run :: ExceptT e (MockLastPassT Identity) a -> (Either e a, [Command])
run = runMockLastPass . runExceptT
