module System.Cli where

import RIO
import qualified RIO.ByteString.Lazy as ByteString
import qualified RIO.Directory as Dir
import qualified RIO.Text as Text
import System.Class (System (..))
import qualified System.Environment as Env
import qualified System.Process as Process
import System.Process.Text (readProcessWithExitCode)

cliSystem :: System
cliSystem =
  System
    { getArgs_ = getArgs,
      getProgName_ = getProgName,
      getHomeDirectory_ = Dir.getHomeDirectory,
      printLine_ = printLine,
      printError_ = printError,
      execInteractive_ = execInteractive,
      exec_ = exec
    }

getArgs :: MonadIO m => m [Text]
getArgs = fmap Text.pack <$> liftIO Env.getArgs

getProgName :: MonadIO m => m Text
getProgName = Text.pack <$> liftIO Env.getProgName

printLine :: MonadIO m => Text -> m ()
printLine = liftIO . ByteString.putStrLn . textToLByteString

printError :: MonadIO m => Text -> m ()
printError = liftIO . ByteString.hPutStr stderr . textToLByteString

execInteractive :: MonadIO m => Text -> m ExitCode
execInteractive = liftIO . Process.system . Text.unpack

exec :: MonadIO m => FilePath -> [Text] -> m (Either () Text)
exec program args = do
  (exitCode, output, _) <- do liftIO $ readProcessWithExitCode program (fmap Text.unpack args) ""
  case exitCode of
    ExitSuccess -> return $ Right output
    ExitFailure _ -> return $ Left ()

textToLByteString :: Text -> LByteString
textToLByteString = ByteString.fromStrict . encodeUtf8
