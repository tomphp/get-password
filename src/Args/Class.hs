module Args.Class (MonadArgs (..), GetArgsError (..)) where

import Control.Monad.Except (ExceptT)
import Control.Monad.Trans (lift)
import Data.Text (Text)
import LastPass.Entry (Search)

newtype GetArgsError = GetArgsError {progName :: Text}

class Monad m => MonadArgs m where
  getSearch :: m (Either GetArgsError Search)

instance (MonadArgs m) => MonadArgs (ExceptT e m) where
  getSearch = lift getSearch
