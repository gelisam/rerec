{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
module ReRec.Audio where

import Control.Concurrent.Async

import System.Process.Async


type Seconds = Double

class Audio a where
  load
    :: FilePath -> IO a
  sox
    :: String  -- ^ destination, i.e. a FilePath or "--default-device"
    -> a
    -> IO (Async ())

instance Audio FilePath where
  load = pure
  sox destination source = execute "sox" [source, destination]


-- cancel the 'Async ()' to stop the recording
record
  :: Audio a
  => FilePath -> IO (Async (), Async a)
record filePath = do
  recordingThread <- execute "sox"
    [ "--default-device"  -- source: the microphone
    , filePath            -- destination: the file
    ]
  loadingThread <- async $ do
    wait recordingThread
    load filePath
  pure (recordingThread, loadingThread)

save
  :: Audio a
  => FilePath -> a -> IO ()
save filePath x = do
  thread <- sox filePath x
  wait thread

play
  :: Audio a
  => a -> IO (Async ())
play = sox "--default-device"

play_
  :: Audio a
  => a -> IO ()
play_ x = do
  thread <- play x
  wait thread
