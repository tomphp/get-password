module Args.CliArgs where

import Args.Class (Args (Args, getSearch_), GetArgsError (GetArgsError))
import LastPass.Entry (Search (Search))
import RIO
import qualified RIO.Text as Text
import System.Class (MonadSystem, getArgs)
import qualified System.Environment as Env

cliArgs :: Args
cliArgs =
  Args
    { getSearch_ = getSearch
    }

getSearch :: (MonadSystem m) => m (Either GetArgsError Search)
getSearch = getArgs >>= extractSeach

extractSeach :: MonadIO m => [Text] -> m (Either GetArgsError Search)
extractSeach = liftM2 maybeToEither (GetArgsError <$> getProgName) . (pure . parseArgs)

parseArgs :: [Text] -> Maybe Search
parseArgs [search] = Just $ Search search
parseArgs _ = Nothing

getProgName :: MonadIO m => m Text
getProgName = Text.pack <$> liftIO Env.getProgName

maybeToEither :: e -> Maybe a -> Either e a
maybeToEither = flip maybe Right . Left
