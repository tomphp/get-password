module LastPassMock
  ( run,
    listPasswordsWillReturn,
    showPasswordWillReturn,
    checkIsInstalledWillErrorWith,
    checkIsLoggedInWillErrorWith,
    listPasswordsWillErrorWith,
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
import Entry (Entry (Entry))
import LastPassClass (MonadLastPass (..))
import LastPassError (LastPassError)

type Command = Text

data Results = Results
  { checkIsInstalledResult :: Either LastPassError (),
    checkIsLoggedInResult :: Either LastPassError (),
    listPasswordsResult :: Either LastPassError [Entry],
    showPasswordResult :: Either LastPassError Text
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
      checkIsLoggedInResult = Right (),
      listPasswordsResult = Right [Entry "default-id" "default-name" "default-url"],
      showPasswordResult = Right ""
    }

mockResult :: Monad m => Text -> (Results -> a) -> MockLastPassT m a
mockResult command getter = do
  tell [command]
  gets getter

instance Monad m => MonadLastPass (MockLastPassT m) where
  checkIsInstalled = do
    mockResult "checkIsInstalled" checkIsInstalledResult

  checkIsLoggedIn = do
    mockResult "checkIsLoggedIn" checkIsLoggedInResult

  listPasswords = do
    mockResult "listPasswords" listPasswordsResult

  showPassword search = do
    mockResult ("showPassword \"" <> search <> "\"") showPasswordResult

listPasswordsWillReturn :: Monad m => [Entry] -> MockLastPassT m ()
listPasswordsWillReturn value = do
  modify $ \state -> state {listPasswordsResult = Right value}

showPasswordWillReturn :: Monad m => Text -> MockLastPassT m ()
showPasswordWillReturn value = do
  modify $ \state -> state {showPasswordResult = Right value}

checkIsInstalledWillErrorWith :: Monad m => LastPassError -> MockLastPassT m ()
checkIsInstalledWillErrorWith err = do
  modify $ \state -> state {checkIsInstalledResult = Left err}

checkIsLoggedInWillErrorWith :: Monad m => LastPassError -> MockLastPassT m ()
checkIsLoggedInWillErrorWith err = do
  modify $ \state -> state {checkIsLoggedInResult = Left err}

listPasswordsWillErrorWith :: Monad m => LastPassError -> MockLastPassT m ()
listPasswordsWillErrorWith err = do
  modify $ \state -> state {listPasswordsResult = Left err}

showPasswordWillErrorWith :: Monad m => LastPassError -> MockLastPassT m ()
showPasswordWillErrorWith err = do
  modify $ \state -> state {showPasswordResult = Left err}

instance MonadLastPass (ExceptT e (MockLastPassT Identity)) where
  checkIsInstalled = lift checkIsInstalled
  checkIsLoggedIn = lift checkIsLoggedIn
  listPasswords = lift listPasswords
  showPassword search = lift $ showPassword search

run :: ExceptT e (MockLastPassT Identity) a -> (Either e a, [Command])
run = runMockLastPass . runExceptT
