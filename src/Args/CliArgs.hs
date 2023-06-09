module Args.CliArgs where

import Args.Class (Args (Args, _getAction, _getSearch), GetArgsError (GetArgsError))
import GetPassword (GetAction (CopyPassword, ShowPassword))
import LastPass.Entry (Search (Search))
import RIO
import qualified RIO.Text as Text
import System.Class (HasSystem, getArgs_)
import qualified System.Environment as Env

cliArgs :: Args
cliArgs =
  Args
    { _getSearch = getSearch,
      _getAction = getAction
    }

getSearch :: (MonadIO m, MonadReader env m, HasSystem env) => m (Either GetArgsError Search)
getSearch = getArgs_ >>= extractSeach

getAction :: (MonadIO m, MonadReader env m, HasSystem env) => m (Either GetArgsError GetAction)
getAction = getArgs_ >>= extractAction

extractSeach :: MonadIO m => [Text] -> m (Either GetArgsError Search)
extractSeach = liftM2 maybeToEither (GetArgsError <$> getProgName) . (pure . fmap fst . parseArgs)

extractAction :: MonadIO m => [Text] -> m (Either GetArgsError GetAction)
extractAction = liftM2 maybeToEither (GetArgsError <$> getProgName) . (pure . fmap snd . parseArgs)

parseArgs :: [Text] -> Maybe (Search, GetAction)
parseArgs ["--show", search] = Just (Search search, ShowPassword)
parseArgs ["--show"] = Nothing
parseArgs [search] = Just (Search search, CopyPassword)
parseArgs _ = Nothing

getProgName :: MonadIO m => m Text
getProgName = Text.pack <$> liftIO Env.getProgName

maybeToEither :: e -> Maybe a -> Either e a
maybeToEither = flip maybe Right . Left
