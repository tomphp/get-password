module GetPassword (getPassword) where

import Control.Monad (void)
import Control.Monad.Except (MonadError (throwError))
import Data.Text (Text)
import Entry (Entry)
import qualified Entry
import LastPass (LastPassError (LastPassMultiplePasswordsFound, LastPassPasswordNotFound), MonadLastPass)
import qualified LastPass

getPassword :: (MonadLastPass m, MonadError LastPassError m) => Text -> m Text
getPassword search = do
  void LastPass.checkIsInstalled
  void LastPass.checkIsLoggedIn

  results <- getMatchingEntries search

  case results of
    [] -> throwError LastPassPasswordNotFound
    [entry] -> LastPass.showPassword (Entry.id entry)
    _ -> throwError (LastPassMultiplePasswordsFound results)

getMatchingEntries :: (MonadLastPass m, MonadError LastPassError m) => Text -> m [Entry]
getMatchingEntries search = filter (Entry.matches search) <$> LastPass.listPasswords
