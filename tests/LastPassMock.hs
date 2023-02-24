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
import Control.Monad.RWS (MonadRWS, MonadState, RWST (runRWST))
import Control.Monad.State (gets, modify)
import Control.Monad.Writer (MonadWriter, tell)
import LastPass.Class (LastPassResult, MonadLastPass (checkIsInstalled, isLoggedIn, listPasswords, login, showPassword), Password (Password), User (User))
import LastPass.Entry (Entry (Entry), EntryID (EntryID))
import LastPass.Error (LastPassError)
import RIO

type Command = Text

data Results = Results
  { checkIsInstalledResult :: !(LastPassResult ()),
    isLoggedInResult :: !Bool,
    loginResult :: !(LastPassResult ()),
    listPasswordsResult :: !(LastPassResult [Entry]),
    showPasswordResult :: !(LastPassResult Password)
  }

type MockLastPass = MockLastPassT Identity

newtype MockLastPassT m a = MockLastPass
  { runMockLastPassT' :: RWST () [Command] Results m a
  }
  deriving stock (Functor)
  deriving newtype
    ( Applicative,
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
      showPasswordResult = Right (Password "")
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

showPasswordWillReturn :: Monad m => Password -> MockLastPassT m ()
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

run :: ExceptT e (MockLastPassT Identity) a -> (Either e a, [Command])
run = runMockLastPass . runExceptT
