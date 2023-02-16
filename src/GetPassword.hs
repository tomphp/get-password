module GetPassword (getPassword) where

import Control.Monad (void)
import Control.Monad.Except (MonadError (throwError))
import Data.Text (Text)
import qualified Entry
import LastPass (LastPassError (LastPassMultiplePasswordsFound, LastPassPasswordNotFound), MonadLastPass (..))

getPassword :: (MonadLastPass m, MonadError LastPassError m) => Text -> m Text
getPassword search = do
  void checkIsInstalled
  void checkIsLoggedIn

  results <- filter (Entry.matches search) <$> listPasswords

  case results of
    [] -> throwError LastPassPasswordNotFound
    [entry] -> showPassword (Entry.id entry)
    _ -> throwError (LastPassMultiplePasswordsFound results)
