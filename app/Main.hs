module Main where

import Control.Monad.Except (runExceptT)
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import qualified GetPassword
import qualified LastPass
import qualified System.Environment as Env

main :: IO ()
main = do
  args <- Env.getArgs
  case args of
    [search] -> do
      result <-
        runExceptT $
          LastPass.runLastPassT $
            GetPassword.getPassword $
              Text.pack search
      case result of
        Left err -> TextIO.putStrLn ("Error: " <> Text.pack (show err))
        Right password -> TextIO.putStrLn password
    _ -> TextIO.putStrLn "Usage: lastpass-hs <search>"
