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
import LastPass
  ( Entry (Entry),
    EntryID (EntryID),
    LastPassError (ListPasswordsFailed, ListPasswordsParseFailed, LoginFailed, NotInstalled, ShowPasswordFailed),
    Password (Password),
    Search (Search),
    User,
  )
import qualified LastPass
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

getArgs :: IO (Maybe Search)
getArgs = parseArgs <$> Env.getArgs

parseArgs :: [String] -> Maybe Search
parseArgs [search] = Just $ Search $ Text.pack search
parseArgs _ = Nothing

runApp :: Maybe User -> Search -> IO ()
runApp user = runGetPassword user >=> either printError printPassword

runGetPassword :: Maybe User -> Search -> IO (Either GetPasswordError Password)
runGetPassword user = runExceptT . LastPass.runLastPassT . GetPassword.getPassword user

---

printPassword :: Password -> IO ()
printPassword (Password password) = TextIO.putStrLn password

printLoadConfigError :: LoadConfigError -> IO ()
printLoadConfigError (LoadConfigError err) = putErrorLn ("Config Error: " <> err)

printError :: GetPasswordError -> IO ()
printError PasswordNotFound = putErrorLn "Error: No matching entries found"
printError NotLoggedIn = putErrorLn "Error: Not logged in. Please login with `lpass login`"
printError (MultiplePasswordsFound entries) = do
  putErrorLn "Error: Multiple entries found:"
  putErrorLn "Matching entries:"
  mapM_ printEntry entries
printError (LastPassErrored err) = printLastPassError err

printLastPassError :: LastPassError -> IO ()
printLastPassError NotInstalled = putErrorLn "Error: LastPass CLI not installed. Please install with `brew install lastpass-cli`"
printLastPassError LoginFailed = putErrorLn "Error: Failed to login in"
printLastPassError ListPasswordsFailed = putErrorLn "Error: Failed to list passwords"
printLastPassError (ListPasswordsParseFailed _) = putErrorLn "Error: Failed to parse list passwords output"
printLastPassError (ShowPasswordFailed _) = putErrorLn "Error: Failed to show password"

printEntry :: Entry -> IO ()
printEntry Entry {id = EntryID entryID, name} = putErrorLn (" - " <> name <> " [" <> entryID <> "]")

printUsage :: IO ()
printUsage = do
  name <- Env.getProgName
  putErrorLn ("Usage: " <> Text.pack name <> " <search>")

putErrorLn :: Text -> IO ()
putErrorLn = TextIO.hPutStrLn stderr
