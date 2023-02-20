module LastPass.Cli (LastPassResult, checkIsInstalled, isLoggedIn, login, listPasswords, showPassword) where

import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Bifunctor (first)
import qualified Data.Either as Either
import Data.Text (Text)
import qualified Data.Text as Text
import LastPass.Entry (Entry)
import qualified LastPass.EntryListParser as EntryListParser
import LastPass.Error (LastPassError)
import qualified LastPass.Error as Error
import System.Exit (ExitCode (ExitSuccess))
import System.Process (system)
import System.Process.Text (readProcessWithExitCode)

type LastPassResult = Either LastPassError

checkIsInstalled :: MonadIO m => m (LastPassResult ())
checkIsInstalled =
  void <$> lpass ["--version"] Error.NotInstalled

checkIsLoggedIn :: MonadIO m => m (Either () ())
checkIsLoggedIn =
  void <$> lpass ["status"] ()

isLoggedIn :: MonadIO m => m Bool
isLoggedIn =
  Either.isRight <$> checkIsLoggedIn

login :: MonadIO m => Text -> m (LastPassResult ())
login user = do
  exitCode <- liftIO $ system ("lpass login " <> Text.unpack user)
  case exitCode of
    ExitSuccess -> return (Right ())
    _ -> return (Left Error.LoginFailed)

listPasswords :: MonadIO m => m (LastPassResult [Entry])
listPasswords = do
  output <- lpass ["ls", "--sync=now", "--format=%ai \"%an\" %al"] Error.ListPasswordsFailed
  return (output >>= parseEntryList)

showPassword :: MonadIO m => Text -> m (LastPassResult Text)
showPassword entryId =
  lpass ["show", "--password", Text.unpack entryId] (Error.ShowPasswordFailed "fixme")

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
