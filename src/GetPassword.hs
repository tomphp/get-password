module GetPassword (getPassword, GetPasswordError (..)) where

import Control.Monad (unless)
import Control.Monad.Except (MonadError (throwError), liftEither)
import Data.Bifunctor (first)
import Data.Text (Text)
import LastPass (Entry, LastPassError, MonadLastPass)
import qualified LastPass
import qualified LastPass.Entry as Entry

data GetPasswordError
  = LastPassErrored LastPassError
  | NotLoggedIn
  | PasswordNotFound
  | MultiplePasswordsFound [Entry]
  deriving (Show, Eq)

getPassword :: (MonadLastPass m, MonadError GetPasswordError m) => Maybe Text -> Text -> m Text
getPassword user search = do
  checkLastPassIsInstalled
  loggedIn <- isLoggedIn

  unless loggedIn $ do
    case user of
      Nothing -> throwError NotLoggedIn
      Just u -> login u

  results <- getMatchingPasswords search

  case results of
    [] -> throwError PasswordNotFound
    [entry] -> showPassword (Entry.id entry)
    _ -> throwError (MultiplePasswordsFound results)

checkLastPassIsInstalled :: (MonadLastPass m, MonadError GetPasswordError m) => m ()
checkLastPassIsInstalled = wrapError LastPass.checkIsInstalled

isLoggedIn :: (MonadLastPass m, MonadError GetPasswordError m) => m Bool
isLoggedIn = LastPass.isLoggedIn

login :: (MonadLastPass m, MonadError GetPasswordError m) => Text -> m ()
login = wrapError . LastPass.login

getMatchingPasswords :: (MonadLastPass m, MonadError GetPasswordError m) => Text -> m [Entry]
getMatchingPasswords search = filter (Entry.matches search) <$> listPasswords

listPasswords :: (MonadLastPass m, MonadError GetPasswordError m) => m [Entry]
listPasswords = wrapError LastPass.listPasswords

showPassword :: (MonadLastPass m, MonadError GetPasswordError m) => Text -> m Text
showPassword = wrapError . LastPass.showPassword

wrapError :: (MonadLastPass m, MonadError GetPasswordError m) => m (Either LastPassError a) -> m a
wrapError = eitherToError LastPassErrored

eitherToError :: (MonadLastPass m, MonadError e' m) => (e -> e') -> m (Either e a) -> m a
eitherToError f = (>>= liftEither . first f)
