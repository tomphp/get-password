module Main where

import Control.Monad ((>=>))
import Control.Monad.Except (runExceptT)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import qualified GetPassword
import GetPassword (GetPasswordError (..))
import qualified LastPass
import qualified System.Environment as Env
import qualified LastPass.Entry as Entry
import LastPass (LastPassError (..))
import System.IO (stderr)

main :: IO ()
main = getArgs >>= maybe printUsage runApp

getArgs :: IO (Maybe Text)
getArgs = parseArgs <$> Env.getArgs

parseArgs :: [String] -> Maybe Text
parseArgs [search] = Just $ Text.pack search
parseArgs _ = Nothing

runApp :: Text -> IO ()
runApp = runGetPassword >=> either printError printPassword

runGetPassword :: Text -> IO (Either GetPasswordError Text)
runGetPassword = runExceptT . LastPass.runLastPassT . GetPassword.getPassword

printPassword :: Text -> IO ()
printPassword = TextIO.putStrLn

printError :: GetPasswordError -> IO ()
printError PasswordNotFound = putErrorLn "Error: No matching entries found"
printError (MultiplePasswordsFound entries) = do
  putErrorLn "Error: Multiple entries found:"
  putErrorLn "Matching entries:"
  mapM_ (\entry -> putErrorLn (" - " <> Entry.name entry <> " [" <> Entry.id entry <> "]")) entries
printError (LastPassErrored err) = printLastPassError err

printLastPassError :: LastPassError -> IO ()
printLastPassError NotInstalled = putErrorLn "Error: LastPass CLI not installed. Please install with `brew install lastpass-cli`"
printLastPassError NotLoggedIn = putErrorLn "Error: Not logged in. Please login with `lpass login`"
printLastPassError ListPasswordsFailed = putErrorLn "Error: Failed to list passwords"
printLastPassError (ListPasswordsParseFailed _) = putErrorLn "Error: Failed to parse list passwords output"
printLastPassError (ShowPasswordFailed _) = putErrorLn "Error: Failed to show password"

printUsage :: IO ()
printUsage = do
  name <- Env.getProgName
  putErrorLn ("Usage: " <> Text.pack name <> " <search>")

putErrorLn :: Text -> IO ()
putErrorLn = TextIO.hPutStrLn stderr
