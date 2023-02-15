module Console.Class (MonadConsole (..)) where

import Data.Text (Text)

class MonadConsole m where
  printLine :: Text -> m ()
  printErrorLine :: Text -> m ()
