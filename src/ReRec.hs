module ReRec
  ( AudioVector, audioVectorDuration, silence, followedBy, overlap
  , AutoTrim(..)
  , Seconds(..)
  , load, play, play_, record, save
  ) where

import Control.Concurrent.Async

import ReRec.AudioVector
import ReRec.AutoTrim
import ReRec.Types
import qualified ReRec.Audio as Audio


load :: FilePath -> IO AudioVector
load = Audio.load

play :: AudioVector -> IO (Async ())
play = Audio.play

play_ :: AudioVector -> IO ()
play_ = Audio.play_

-- |
-- cancel the 'Async ()' to stop the recording
record :: FilePath -> IO (Async (), Async AudioVector)
record = Audio.record

save :: FilePath -> AudioVector -> IO ()
save = Audio.save
