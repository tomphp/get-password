module LastPass.Cli (checkIsInstalled, checkIsLoggedIn, listPasswords, showPassword) where

import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Bifunctor (first)
import Data.Text (Text)
import qualified Data.Text as Text
import LastPass.Entry (Entry)
import qualified LastPass.EntryListParser as EntryListParser
import LastPass.Error (LastPassError)
import qualified LastPass.Error as Error
import System.Exit (ExitCode (ExitSuccess))
import System.Process.Text (readProcessWithExitCode)

checkIsInstalled :: MonadIO m => m (Either LastPassError ())
checkIsInstalled =
  void <$> lpass ["--version"] Error.NotInstalled

checkIsLoggedIn :: MonadIO m => m (Either LastPassError ())
checkIsLoggedIn =
  void <$> lpass ["status"] Error.NotLoggedIn

listPasswords :: MonadIO m => m (Either LastPassError [Entry])
listPasswords = do
  output <- lpass ["ls", "--sync=now", "--format=%ai \"%an\" %al"] Error.ListPasswordsFailed
  return (output >>= parseEntryList)

showPassword :: MonadIO m => Text -> m (Either LastPassError Text)
showPassword entryId =
  lpass ["show", "--password", Text.unpack entryId] (Error.ShowPasswordFailed "fixme")

parseEntryList :: Text -> Either LastPassError [Entry]
parseEntryList = first Error.ListPasswordsParseFailed . EntryListParser.parse

lpass :: MonadIO m => [String] -> e -> m (Either e Text)
lpass = runOrError "lpass"

runOrError :: MonadIO m => FilePath -> [String] -> e -> m (Either e Text)
runOrError cmd args err = do
  (exitCode, output, _) <- liftIO $ readProcessWithExitCode cmd args ""
  case exitCode of
    ExitSuccess -> return (Right output)
    _ -> return (Left err)
