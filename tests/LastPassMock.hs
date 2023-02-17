module LastPassMock
  ( run,
    checkIsInstalledWillReturn,
    checkIsLoggedInWillReturn,
    listPasswordsWillReturn,
    showPasswordWillReturn,
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

type LastPassResult a = Either LastPassError a

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

checkIsInstalledWillReturn :: Monad m => LastPassResult () -> MockLastPassT m ()
checkIsInstalledWillReturn returnValue = do
  modify $ \state -> state {checkIsInstalledResult = returnValue}

checkIsLoggedInWillReturn :: Monad m => LastPassResult () -> MockLastPassT m ()
checkIsLoggedInWillReturn returnValue = do
  modify $ \state -> state {checkIsLoggedInResult = returnValue}

listPasswordsWillReturn :: Monad m => LastPassResult [Entry] -> MockLastPassT m ()
listPasswordsWillReturn returnValue = do
  modify $ \state -> state {listPasswordsResult = returnValue}

showPasswordWillReturn :: Monad m => LastPassResult Text -> MockLastPassT m ()
showPasswordWillReturn returnValue = do
  modify $ \state -> state {showPasswordResult = returnValue}

instance MonadLastPass (ExceptT e (MockLastPassT Identity)) where
  checkIsInstalled = lift checkIsInstalled
  checkIsLoggedIn = lift checkIsLoggedIn
  listPasswords = lift listPasswords
  showPassword search = lift $ showPassword search

run :: ExceptT e (MockLastPassT Identity) a -> (Either e a, [Command])
run = runMockLastPass . runExceptT
