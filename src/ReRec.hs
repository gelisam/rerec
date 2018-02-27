module ReRec
  ( Audio(load), record, save, play, play_
  , AudioVector, audioVectorDuration, silence, followedBy, overlap
  , AutoTrim(..)
  , Seconds(..)
  ) where

import ReRec.Audio
import ReRec.AudioVector
import ReRec.AutoTrim
import ReRec.Types
