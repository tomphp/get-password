module LastPass.Cli (checkIsInstalled, isLoggedIn, login, listPasswords, showPassword) where

import LastPass.Class (LastPassResult, Password (Password), User (User))
import LastPass.Entry (Entry, EntryID (EntryID))
import qualified LastPass.EntryListParser as EntryListParser
import qualified LastPass.Error as Error
import RIO
import qualified RIO.Text as Text
import System.Process (system)
import System.Process.Text (readProcessWithExitCode)

checkIsInstalled :: MonadIO m => m (LastPassResult ())
checkIsInstalled =
  void <$> lpass ["--version"] Error.NotInstalled

checkIsLoggedIn :: MonadIO m => m (Either () ())
checkIsLoggedIn =
  void <$> lpass ["status"] ()

isLoggedIn :: MonadIO m => m Bool
isLoggedIn =
  isRight <$> checkIsLoggedIn

login :: MonadIO m => User -> m (LastPassResult ())
login (User user) = do
  exitCode <- liftIO $ system ("lpass login " <> Text.unpack user)
  case exitCode of
    ExitSuccess -> return (Right ())
    _ -> return (Left Error.LoginFailed)

listPasswords :: MonadIO m => m (LastPassResult [Entry])
listPasswords = do
  output <- lpass ["ls", "--sync=now", "--format=%ai \"%an\" %al"] Error.ListPasswordsFailed
  return (output >>= parseEntryList)

showPassword :: MonadIO m => EntryID -> m (LastPassResult Password)
showPassword (EntryID entryId) = do
  result <- lpass ["show", "--password", Text.unpack entryId] (Error.ShowPasswordFailed "fixme")
  return (Password <$> result)

parseEntryList :: Text -> LastPassResult [Entry]
parseEntryList = first Error.ListPasswordsParseFailed . EntryListParser.parse

lpass :: MonadIO m => [String] -> e -> m (Either e Text)
lpass = runOrError "lpass"

runOrError :: MonadIO m => FilePath -> [String] -> e -> m (Either e Text)
runOrError cmd args err = do
  (exitCode, output, _) <- liftIO $ readProcessWithExitCode cmd args ""
  case exitCode of
    ExitSuccess -> return (Right output)
    _ -> return (Left err)
