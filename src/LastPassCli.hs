module LastPassCli (checkIsInstalled, checkIsLoggedIn, listPasswords, showPassword) where

import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Bifunctor (first)
import Data.Text (Text)
import qualified Data.Text as Text
import Entry (Entry)
import qualified EntryListParser
import LastPassError
  ( LastPassError
      ( ListPasswordsFailed,
        ListPasswordsParseFailed,
        NotInstalled,
        NotLoggedIn,
        ShowPasswordFailed
      ),
  )
import System.Exit (ExitCode (ExitSuccess))
import System.Process.Text (readProcessWithExitCode)

checkIsInstalled :: MonadIO m => m (Either LastPassError ())
checkIsInstalled =
  void <$> lpass ["--version"] NotInstalled

checkIsLoggedIn :: MonadIO m => m (Either LastPassError ())
checkIsLoggedIn =
  void <$> lpass ["status"] NotLoggedIn

listPasswords :: MonadIO m => m (Either LastPassError [Entry])
listPasswords = do
  output <- lpass ["ls", "--sync=now", "--format=%ai \"%an\" %al"] ListPasswordsFailed
  return (output >>= parseEntryList)

showPassword :: MonadIO m => Text -> m (Either LastPassError Text)
showPassword entryId =
  lpass ["show", "--password", Text.unpack entryId] (ShowPasswordFailed "fixme")

parseEntryList :: Text -> Either LastPassError [Entry]
parseEntryList = first ListPasswordsParseFailed . EntryListParser.parse

lpass :: MonadIO m => [String] -> e -> m (Either e Text)
lpass = runOrError "lpass"

runOrError :: MonadIO m => FilePath -> [String] -> e -> m (Either e Text)
runOrError cmd args err = do
  (exitCode, output, _) <- liftIO $ readProcessWithExitCode cmd args ""
  case exitCode of
    ExitSuccess -> return (Right output)
    _ -> return (Left err)
