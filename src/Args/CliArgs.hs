module Args.CliArgs where

import Args.Class (Args (Args, getSearch_), GetArgsError (GetArgsError))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text (Text)
import qualified Data.Text as Text
import LastPass.Entry (Search (Search))
import qualified System.Environment as Env

cliArgs :: Args
cliArgs =
  Args
    { getSearch_ = getSearch
    }

getSearch :: MonadIO m => m (Either GetArgsError Search)
getSearch = do
  args <- liftIO Env.getArgs
  parseArgs args
  where
    parseArgs [search] = return $ Right $ Search $ Text.pack search
    parseArgs _ = Left . GetArgsError <$> getProgName

getProgName :: MonadIO m => m Text
getProgName = Text.pack <$> liftIO Env.getProgName