module System.Process.Async where

import Control.Concurrent.Async
import Control.Exception
import System.Process.Typed


execute :: FilePath -> [String] -> IO (Async ())
execute cmd args = do
  let processConfig = setStdin closed
                    . setStdout closed
                    . setStderr closed
                    $ proc cmd args
  process <- startProcess processConfig
  async $ do
    r <- try $ checkExitCode process
    case r :: Either SomeException () of
      Right () -> pure ()
      Left e -> do
        stopProcess process
        throwIO e

execute_ :: FilePath -> [String] -> IO ()
execute_ cmd args = do
  thread <- execute cmd args
  wait thread
