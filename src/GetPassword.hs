module GetPassword (getPassword) where

import Control.Monad (void)
import Control.Monad.Except (MonadError (throwError))
import Data.Text (Text, isInfixOf)
import LastPass (LastPassError (LastPassMultiplePasswordsFound, LastPassPasswordNotFound), MonadLastPass (..))
import PasswordEntry (PasswordEntry)
import qualified PasswordEntry as Entry

getPassword :: (MonadLastPass m, MonadError LastPassError m) => Text -> m Text
getPassword search = do
  void checkIsInstalled
  void checkIsLoggedIn

  results <- filter (matches search) <$> listPasswords

  case results of
    [] -> throwError LastPassPasswordNotFound
    [entry] -> showPassword (Entry.id entry)
    _ -> throwError (LastPassMultiplePasswordsFound results)

matches :: Text -> PasswordEntry -> Bool
matches search entry = search `isInfixOf` Entry.name entry || search `isInfixOf` Entry.url entry
