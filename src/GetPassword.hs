module GetPassword (getPassword, GetPasswordError (..)) where

import Control.Monad (unless)
import Control.Monad.Except (MonadError (throwError), liftEither)
import qualified Data.Bifunctor as Bifunctor
import Data.Text (Text)
import LastPass (Entry, LastPassError, LastPassResult, MonadLastPass, Search, User)
import qualified LastPass
import LastPass.Entry (EntryID)
import qualified LastPass.Entry as Entry

data GetPasswordError
  = LastPassErrored LastPassError
  | NotLoggedIn
  | PasswordNotFound
  | MultiplePasswordsFound [Entry]
  deriving (Show, Eq)

getPassword :: (MonadLastPass m, MonadError GetPasswordError m) => Maybe User -> Search -> m Text
getPassword user search = do
  checkLastPassIsInstalled
  loggedIn <- isLoggedIn
  unless loggedIn (attemptLogin user)
  results <- getMatchingPasswords search
  case results of
    [] -> throwError PasswordNotFound
    [entry] -> showPassword (Entry.id entry)
    _ -> throwError (MultiplePasswordsFound results)

checkLastPassIsInstalled :: (MonadLastPass m, MonadError GetPasswordError m) => m ()
checkLastPassIsInstalled = wrapError LastPass.checkIsInstalled

isLoggedIn :: (MonadLastPass m, MonadError GetPasswordError m) => m Bool
isLoggedIn = LastPass.isLoggedIn

attemptLogin :: (MonadLastPass m, MonadError GetPasswordError m) => Maybe User -> m ()
attemptLogin = maybe (throwError NotLoggedIn) login

login :: (MonadLastPass m, MonadError GetPasswordError m) => User -> m ()
login = wrapError . LastPass.login

getMatchingPasswords :: (MonadLastPass m, MonadError GetPasswordError m) => Search -> m [Entry]
getMatchingPasswords search = filter (Entry.matches search) <$> listPasswords

listPasswords :: (MonadLastPass m, MonadError GetPasswordError m) => m [Entry]
listPasswords = wrapError LastPass.listPasswords

showPassword :: (MonadLastPass m, MonadError GetPasswordError m) => EntryID -> m Text
showPassword = wrapError . LastPass.showPassword

wrapError :: (MonadLastPass m, MonadError GetPasswordError m) => m (LastPassResult a) -> m a
wrapError = eitherToError LastPassErrored

eitherToError :: (MonadLastPass m, MonadError e' m) => (e -> e') -> m (Either e a) -> m a
eitherToError f = (>>= liftEither . Bifunctor.first f)
