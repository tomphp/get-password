module Main where

import Control.Monad.Except (runExceptT)
import qualified GetPassword (getPassword)
import qualified LastPass (runLastPassT)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [search] -> do
      result <- runExceptT $ LastPass.runLastPassT $ GetPassword.getPassword search
      case result of
        Left err -> putStrLn ("Error: " <> show err)
        Right password -> putStrLn ("Password is " <> password)
    _ -> putStrLn "Usage: lastpass-hs <search>"
