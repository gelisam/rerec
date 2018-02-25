{-# LANGUAGE FlexibleInstances, RecordWildCards, TemplateHaskell, TypeSynonymInstances #-}
module ReRec.Audio where

import Control.Concurrent.Async
import Control.Lens

import qualified ReRec.Sox as Sox


data Sox = Sox
  { _soxSource :: Sox.Source
  , _soxFilter :: Sox.Filter
  } deriving Show

makeLenses ''Sox

runSox
  :: Sox -> Sox.Destination -> IO (Async ())
runSox (Sox {..}) destination = Sox.run _soxSource destination _soxFilter

runSox_
  :: Sox -> Sox.Destination -> IO ()
runSox_ sox destination = do
  thread <- runSox sox destination
  wait thread


class Audio a where
  load
    :: FilePath -> IO a
  toSox
    :: a -> Sox

instance Audio FilePath where
  load = pure
  toSox filePath = Sox (Sox.fileSource filePath) mempty

instance Audio Sox where
  load = pure . toSox
  toSox = id


-- cancel the 'Async ()' to stop the recording
record
  :: Audio a
  => FilePath -> IO (Async (), Async a)
record filePath = do
  recordingThread <- Sox.run Sox.microphoneSource
                             (Sox.fileDestination filePath)
                             mempty
  loadingThread <- async $ do
    wait recordingThread
    load filePath
  pure (recordingThread, loadingThread)

save
  :: Audio a
  => FilePath -> a -> IO ()
save filePath x = runSox_ (toSox x) (Sox.fileDestination filePath)

play
  :: Audio a
  => a -> IO (Async ())
play x = runSox (toSox x) Sox.speakersDestination

play_
  :: Audio a
  => a -> IO ()
play_ x = do
  thread <- play x
  wait thread
