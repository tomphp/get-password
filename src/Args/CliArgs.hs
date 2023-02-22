module Args.CliArgs where

import Args.Class (GetArgsError (GetArgsError), MonadArgs (..))
import Console.Class (MonadConsole)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans (MonadTrans, lift)
import Data.Text (Text)
import qualified Data.Text as Text
import LastPass.Entry (Search (Search))
import qualified System.Environment as Env

newtype CliArgsT m a = CliArgsT {runCliArgsT :: m a}
  deriving stock (Functor)
  deriving newtype (Applicative, Monad, MonadConsole, MonadIO)

instance MonadTrans CliArgsT where
  lift = CliArgsT

instance MonadIO m => MonadArgs (CliArgsT m) where
  getSearch = getSearch'

getSearch' :: MonadIO m => m (Either GetArgsError Search)
getSearch' = do
  args <- liftIO Env.getArgs
  parseArgs args
  where
    parseArgs [search] = return $ Right $ Search $ Text.pack search
    parseArgs _ = Left . GetArgsError <$> getProgName

getProgName :: MonadIO m => m Text
getProgName = Text.pack <$> liftIO Env.getProgName
