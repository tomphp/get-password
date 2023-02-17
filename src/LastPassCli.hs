module LastPassCli (checkIsInstalled, checkIsLoggedIn, listPasswords, showPassword) where

import Control.Monad (void)
import Data.Bifunctor (first)
import Data.Text (Text)
import qualified Data.Text as Text
import Entry (Entry)
import qualified EntryListParser
import LastPassError
  ( LastPassError
      ( LastPassListPasswordsFailed,
        LastPassListPasswordsParseFailed,
        LastPassNotInstalled,
        LastPassNotLoggedIn,
        LastPassShowPasswordFailed
      ),
  )
import System.Exit (ExitCode (ExitSuccess))
import System.Process.Text (readProcessWithExitCode)

checkIsInstalled :: IO (Either LastPassError ())
checkIsInstalled =
  void <$> lpass ["--version"] LastPassNotInstalled

checkIsLoggedIn :: IO (Either LastPassError ())
checkIsLoggedIn =
  void <$> lpass ["status"] LastPassNotLoggedIn

listPasswords :: IO (Either LastPassError [Entry])
listPasswords =
  (>>= parseEntryList) <$> lpass ["ls", "--sync=now", "--format=%ai \"%an\" %al"] LastPassListPasswordsFailed

showPassword :: Text -> IO (Either LastPassError Text)
showPassword entryId =
  lpass ["show", "--password", Text.unpack entryId] (LastPassShowPasswordFailed "fixme")

parseEntryList :: Text -> Either LastPassError [Entry]
parseEntryList = first LastPassListPasswordsParseFailed . EntryListParser.parse

lpass :: [String] -> LastPassError -> IO (Either LastPassError Text)
lpass = run "lpass"

run :: FilePath -> [String] -> e -> IO (Either e Text)
run cmd args err = do
  (exitCode, output, _) <- readProcessWithExitCode cmd args ""
  case exitCode of
    ExitSuccess -> return (Right output)
    _ -> return (Left err)
