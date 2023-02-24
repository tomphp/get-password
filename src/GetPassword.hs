module GetPassword (getPassword, GetPasswordError (..)) where

import Control.Monad.Except (MonadError, throwError, liftEither)
import LastPass.Class (LastPassResult, MonadLastPass, Password, User)
import qualified LastPass.Class as LastPass
import LastPass.Entry (Entry, EntryID, Search)
import qualified LastPass.Entry as Entry
import LastPass.Error (LastPassError)
import RIO

data GetPasswordError
  = LastPassErrored !LastPassError
  | NotLoggedIn
  | PasswordNotFound
  | MultiplePasswordsFound ![Entry]
  deriving stock (Show, Eq)

getPassword :: (MonadLastPass m, MonadError GetPasswordError m) => Maybe User -> Search -> m Password
getPassword user search = do
  checkLastPassIsInstalled
  loggedIn <- isLoggedIn
  unless loggedIn (attemptLogin user)
  entries <- getMatchingPasswords search
  entryId <- getEntryId entries
  showPassword entryId

checkLastPassIsInstalled :: (MonadLastPass m, MonadError GetPasswordError m) => m ()
checkLastPassIsInstalled = wrapError LastPass.checkIsInstalled

isLoggedIn :: (MonadLastPass m) => m Bool
isLoggedIn = LastPass.isLoggedIn

attemptLogin :: (MonadLastPass m, MonadError GetPasswordError m) => Maybe User -> m ()
attemptLogin = maybe (throwError NotLoggedIn) login

login :: (MonadLastPass m, MonadError GetPasswordError m) => User -> m ()
login = wrapError . LastPass.login

getMatchingPasswords :: (MonadLastPass m, MonadError GetPasswordError m) => Search -> m [Entry]
getMatchingPasswords search = filter (Entry.matches search) <$> listPasswords

listPasswords :: (MonadLastPass m, MonadError GetPasswordError m) => m [Entry]
listPasswords = wrapError LastPass.listPasswords

getEntryId :: MonadError GetPasswordError m => [Entry] -> m EntryID
getEntryId [] = throwError PasswordNotFound
getEntryId [entry] = return (Entry.id entry)
getEntryId entries = throwError (MultiplePasswordsFound entries)

showPassword :: (MonadLastPass m, MonadError GetPasswordError m) => EntryID -> m Password
showPassword = wrapError . LastPass.showPassword

wrapError :: (MonadLastPass m, MonadError GetPasswordError m) => m (LastPassResult a) -> m a
wrapError = eitherToError LastPassErrored

eitherToError :: (MonadLastPass m, MonadError e' m) => (e -> e') -> m (Either e a) -> m a
eitherToError f = (>>= liftEither . first f)
