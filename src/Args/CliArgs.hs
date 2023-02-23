module Args.CliArgs where

import Args.Class (Args (Args, getSearch_), GetArgsError (GetArgsError))
import LastPass.Entry (Search (Search))
import RIO
import qualified RIO.Text as Text
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
