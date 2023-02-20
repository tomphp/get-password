module Main where

import Config (Config (Config), LoadConfigError (LoadConfigError))
import qualified Config
import Control.Monad ((>=>))
import Control.Monad.Except (runExceptT)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import GetPassword (GetPasswordError (..))
import qualified GetPassword
import LastPass (LastPassError (ListPasswordsFailed, ListPasswordsParseFailed, LoginFailed, NotInstalled, ShowPasswordFailed))
import qualified LastPass
import qualified LastPass.Entry as Entry
import qualified System.Environment as Env
import System.IO (stderr)

main :: IO ()
main = do
  config <- Config.loadConfig
  case config of
    Left err -> printLoadConfigError err
    Right Config {user} -> do
      args <- getArgs
      maybe printUsage (runApp user) args

getArgs :: IO (Maybe Text)
getArgs = parseArgs <$> Env.getArgs

parseArgs :: [String] -> Maybe Text
parseArgs [search] = Just $ Text.pack search
parseArgs _ = Nothing

runApp :: Maybe Text -> Text -> IO ()
runApp user = runGetPassword user >=> either printError printPassword

runGetPassword :: Maybe Text -> Text -> IO (Either GetPasswordError Text)
runGetPassword user = runExceptT . LastPass.runLastPassT . GetPassword.getPassword user

---

printPassword :: Text -> IO ()
printPassword = TextIO.putStrLn

printLoadConfigError :: LoadConfigError -> IO ()
printLoadConfigError (LoadConfigError err) = putErrorLn ("Config Error: " <> err)

printError :: GetPasswordError -> IO ()
printError PasswordNotFound = putErrorLn "Error: No matching entries found"
printError NotLoggedIn = putErrorLn "Error: Not logged in. Please login with `lpass login`"
printError (MultiplePasswordsFound entries) = do
  putErrorLn "Error: Multiple entries found:"
  putErrorLn "Matching entries:"
  mapM_ (\entry -> putErrorLn (" - " <> Entry.name entry <> " [" <> Entry.id entry <> "]")) entries
printError (LastPassErrored err) = printLastPassError err

printLastPassError :: LastPassError -> IO ()
printLastPassError NotInstalled = putErrorLn "Error: LastPass CLI not installed. Please install with `brew install lastpass-cli`"
printLastPassError LoginFailed = putErrorLn "Error: Failed to login in"
printLastPassError ListPasswordsFailed = putErrorLn "Error: Failed to list passwords"
printLastPassError (ListPasswordsParseFailed _) = putErrorLn "Error: Failed to parse list passwords output"
printLastPassError (ShowPasswordFailed _) = putErrorLn "Error: Failed to show password"

printUsage :: IO ()
printUsage = do
  name <- Env.getProgName
  putErrorLn ("Usage: " <> Text.pack name <> " <search>")

putErrorLn :: Text -> IO ()
putErrorLn = TextIO.hPutStrLn stderr
