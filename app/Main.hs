module Main where

import Control.Monad ((>=>))
import Control.Monad.Except (runExceptT)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import qualified GetPassword
import qualified LastPass
import LastPassError (LastPassError)
import qualified System.Environment as Env

main :: IO ()
main = getArgs >>= maybe printUsage runApp

getArgs :: IO (Maybe Text)
getArgs = parseArgs <$> Env.getArgs

parseArgs :: [String] -> Maybe Text
parseArgs [search] = Just $ Text.pack search
parseArgs _ = Nothing

runApp :: Text -> IO ()
runApp = runGetPassword >=> either printError printPassword

runGetPassword :: Text -> IO (Either LastPassError Text)
runGetPassword = runExceptT . LastPass.runLastPassT . GetPassword.getPassword

printPassword :: Text -> IO ()
printPassword = TextIO.putStrLn

printError :: LastPassError -> IO ()
printError err = TextIO.putStrLn ("Error: " <> Text.pack (show err))

printUsage :: IO ()
printUsage = do
  name <- Env.getProgName
  TextIO.putStrLn ("Usage: " <> Text.pack name <> " <search>")
