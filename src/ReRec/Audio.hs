{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
module ReRec.Audio where

import Control.Concurrent.Async

import ReRec.Sox (Sox, runSox, runSox_)
import qualified ReRec.Sox as Sox


class Audio a where
  load
    :: FilePath -> IO a
  toSox
    :: a -> Sox

instance Audio FilePath where
  load = pure
  toSox = Sox.file

instance Audio Sox where
  load = pure . Sox.file
  toSox = id


-- cancel the 'Async ()' to stop the recording
record
  :: Audio a
  => FilePath -> IO (Async (), Async a)
record filePath = do
  recordingThread <- runSox Sox.rec filePath
  loadingThread <- async $ do
    wait recordingThread
    load filePath
  pure (recordingThread, loadingThread)

save
  :: Audio a
  => FilePath -> a -> IO ()
save filePath x = runSox_ (toSox x) filePath

play
  :: Audio a
  => a -> IO (Async ())
play x = runSox (toSox x) "--default-device"

play_
  :: Audio a
  => a -> IO ()
play_ x = do
  thread <- play x
  wait thread
