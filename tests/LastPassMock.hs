module LastPassMock
  ( runMock,
    checkIsInstalledWillReturn,
    checkIsLoggedInWillReturn,
    listPasswordsWillReturn,
    showPasswordWillReturn,
  )
where

import Control.Monad.Except (ExceptT, MonadError, liftEither, runExceptT)
import Control.Monad.RWS (MonadState, RWS, runRWS)
import Control.Monad.State (get, modify)
import Control.Monad.Writer (MonadWriter, tell)
import Data.Text (Text)
import Entry (Entry (Entry))
import LastPass (LastPassError, MonadLastPass (..))

type Command = Text

data Results = Results
  { checkIsInstalledResult :: Either LastPassError (),
    checkIsLoggedInResult :: Either LastPassError (),
    listPasswordsResult :: Either LastPassError [Entry],
    showPasswordResult :: Either LastPassError Text
  }

newtype MockLastPass a = MockLastPass
  { runMockLastPass :: ExceptT LastPassError (RWS () [Command] Results) a
  }
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadWriter [Command],
      MonadState Results,
      MonadError LastPassError
    )

type LastPassResult a = Either LastPassError a

defaultResults :: Results
defaultResults =
  Results
    { checkIsInstalledResult = Right (),
      checkIsLoggedInResult = Right (),
      listPasswordsResult = Right [Entry "default-id" "default-name" "default-url"],
      showPasswordResult = Right ""
    }

mockResult :: Text -> (Results -> Either LastPassError a) -> MockLastPass a
mockResult command getter = do
  tell [command]
  results <- get
  liftEither $ getter results

instance MonadLastPass MockLastPass where
  checkIsInstalled = do
    mockResult "checkIsInstalled" checkIsInstalledResult

  checkIsLoggedIn = do
    mockResult "checkIsLoggedIn" checkIsLoggedInResult

  listPasswords = do
    mockResult "listPasswords" listPasswordsResult

  showPassword search = do
    mockResult ("showPassword \"" <> search <> "\"") showPasswordResult

checkIsInstalledWillReturn :: LastPassResult () -> MockLastPass ()
checkIsInstalledWillReturn returnValue = do
  modify $ \state -> state {checkIsInstalledResult = returnValue}

checkIsLoggedInWillReturn :: LastPassResult () -> MockLastPass ()
checkIsLoggedInWillReturn returnValue = do
  modify $ \state -> state {checkIsLoggedInResult = returnValue}

listPasswordsWillReturn :: LastPassResult [Entry] -> MockLastPass ()
listPasswordsWillReturn returnValue = do
  modify $ \state -> state {listPasswordsResult = returnValue}

showPasswordWillReturn :: LastPassResult Text -> MockLastPass ()
showPasswordWillReturn returnValue = do
  modify $ \state -> state {showPasswordResult = returnValue}

runMock :: MockLastPass a -> (LastPassResult a, [Command])
runMock mock =
  let (password, _, history) = runRWS (runExceptT (runMockLastPass mock)) () defaultResults
   in (password, history)
