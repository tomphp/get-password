module GetPassword (getPassword, GetAction (..), GetPasswordResult (..), GetPasswordError (..)) where

import Control.Monad.Except (MonadError, liftEither, throwError)
import LastPass.Class (HasLastPass, LastPassResult, Password, User)
import qualified LastPass.Class as LastPass
import LastPass.Entry (Entry, EntryID, Search)
import qualified LastPass.Entry as Entry
import LastPass.Error (LastPassError)
import RIO

data GetAction = ShowPassword | CopyPassword deriving (Eq, Show)

data GetPasswordResult = CopiedPassword | FetchedPassword Password

data GetPasswordError
  = LastPassErrored !LastPassError
  | NotLoggedIn
  | PasswordNotFound
  | MultiplePasswordsFound ![Entry]
  deriving stock (Show, Eq)

getPassword :: (MonadIO m, MonadReader env m, HasLastPass env, MonadError GetPasswordError m) => Maybe User -> GetAction -> Search -> m GetPasswordResult
getPassword user action search = do
  checkLastPassIsInstalled
  loggedIn <- isLoggedIn
  unless loggedIn (attemptLogin user)
  entries <- getMatchingPasswords search
  entryId <- getEntryId entries
  if action == ShowPassword
    then FetchedPassword <$> showPassword entryId
    else copyPassword entryId >> pure CopiedPassword

checkLastPassIsInstalled :: (MonadIO m, MonadReader env m, HasLastPass env, MonadError GetPasswordError m) => m ()
checkLastPassIsInstalled = wrapError LastPass.checkIsInstalled_

isLoggedIn :: (MonadIO m, MonadReader env m, HasLastPass env) => m Bool
isLoggedIn = LastPass.isLoggedIn_

attemptLogin :: (MonadIO m, MonadReader env m, HasLastPass env, MonadError GetPasswordError m) => Maybe User -> m ()
attemptLogin = maybe (throwError NotLoggedIn) login

login :: (MonadIO m, MonadReader env m, HasLastPass env, MonadError GetPasswordError m) => User -> m ()
login = wrapError . LastPass.login_

getMatchingPasswords :: (MonadIO m, MonadReader env m, HasLastPass env, MonadError GetPasswordError m) => Search -> m [Entry]
getMatchingPasswords search = filter (Entry.matches search) <$> listPasswords

listPasswords :: (MonadIO m, MonadReader env m, HasLastPass env, MonadError GetPasswordError m) => m [Entry]
listPasswords = wrapError LastPass.listPasswords_

getEntryId :: MonadError GetPasswordError m => [Entry] -> m EntryID
getEntryId [] = throwError PasswordNotFound
getEntryId [entry] = return (Entry.id entry)
getEntryId entries = throwError (MultiplePasswordsFound entries)

showPassword :: (MonadIO m, MonadReader env m, HasLastPass env, MonadError GetPasswordError m) => EntryID -> m Password
showPassword = wrapError . LastPass.showPassword_

copyPassword :: (MonadIO m, MonadReader env m, HasLastPass env, MonadError GetPasswordError m) => EntryID -> m ()
copyPassword = wrapError . LastPass.copyPassword_

wrapError :: (MonadReader env m, MonadError GetPasswordError m) => m (LastPassResult a) -> m a
wrapError = eitherToError LastPassErrored

eitherToError :: (MonadReader env m, MonadError e' m) => (e -> e') -> m (Either e a) -> m a
eitherToError f = (>>= liftEither . first f)
