module GetPassword (getPassword) where

import Control.Monad (void)
import Control.Monad.Except (MonadError (throwError))
import Data.List (isInfixOf)
import LastPass (LastPassError (LastPassMultiplePasswordsFound, LastPassPasswordNotFound), MonadLastPass (..))
import PasswordEntry (PasswordEntry (entryId, name, url))

getPassword :: (MonadLastPass m, MonadError LastPassError m) => String -> m String
getPassword search = do
  void checkIsInstalled
  void checkIsLoggedIn

  results <- filter (matches search) <$> listPasswords

  case results of
    [] -> throwError LastPassPasswordNotFound
    [entry] -> showPassword (entryId entry)
    _ -> throwError (LastPassMultiplePasswordsFound results)

matches :: String -> PasswordEntry -> Bool
matches search entry = search `isInfixOf` name entry || search `isInfixOf` url entry
