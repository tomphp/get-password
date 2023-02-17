module GetPassword (getPassword) where

import Control.Monad.Except (MonadError (throwError), liftEither)
import Data.Bifunctor (first)
import Data.Text (Text)
import GetPasswordError (GetPasswordError (LastPassErrored, MultiplePasswordsFound, PasswordNotFound))
import LastPass.Class (MonadLastPass)
import qualified LastPass.Class as LastPass
import LastPass.Entry (Entry)
import qualified LastPass.Entry as Entry
import LastPass.Error (LastPassError)

getPassword :: (MonadLastPass m, MonadError GetPasswordError m) => Text -> m Text
getPassword search = do
  checkLastPassIsInstalled
  checkLastPassIsLoggedIn

  results <- getMatchingPasswords search

  case results of
    [] -> throwError PasswordNotFound
    [entry] -> showPassword (Entry.id entry)
    _ -> throwError (MultiplePasswordsFound results)

checkLastPassIsInstalled :: (MonadLastPass m, MonadError GetPasswordError m) => m ()
checkLastPassIsInstalled = wrapError LastPass.checkIsInstalled

checkLastPassIsLoggedIn :: (MonadLastPass m, MonadError GetPasswordError m) => m ()
checkLastPassIsLoggedIn = wrapError LastPass.checkIsLoggedIn

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
