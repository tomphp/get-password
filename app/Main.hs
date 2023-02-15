module Main where

import Control.Monad.Except (runExceptT)
import Data.Text (pack)
import qualified Data.Text.IO as TextIO
import qualified GetPassword (getPassword)
import qualified LastPass (runLastPassT)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [search] -> do
      result <- runExceptT $ LastPass.runLastPassT $ GetPassword.getPassword $ pack search
      case result of
        Left err -> TextIO.putStrLn ("Error: " <> pack (show err))
        Right password -> TextIO.putStrLn ("Password is " <> password)
    _ -> TextIO.putStrLn "Usage: lastpass-hs <search>"
