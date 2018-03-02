module System.Process.Extra where

import Control.Monad.STM
import System.Process.Typed
import qualified Data.Text.Lazy as Text
import qualified Data.Text.Lazy.Encoding as Text


readProcessLn :: Read a => FilePath -> [String] -> IO a
readProcessLn cmd args = do
  let processConfig = setStdin closed
                    . setStdout byteStringOutput
                    . setStderr closed
                    $ proc cmd args
  withProcess_ processConfig $ \process -> do
    atomically $ do
      read . Text.unpack . Text.decodeUtf8 <$> getStdout process
