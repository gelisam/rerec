{-# LANGUAGE RecordWildCards, TemplateHaskell #-}
module ReRec.Segment where

import Control.Lens
import Data.List
import ReRec.Audio
import ReRec.Sox as Sox


-- |
-- A portion of an audio clip, except that the selected segment need not be
-- completely included within said clip. If the offset is negative or the
-- duration extends beyond the clip's, the remainder will be filled with
-- silence.
data Segment = Segment
  { _segmentSource             :: Source
  , _segmentSourceDuration     :: Seconds  -- ^ non-negative
  , _segmentSourceChannelCount :: Int      -- ^ should be 1 or 2
  , _segmentSourceSampleRate   :: Hz
  , _segmentOffset             :: Seconds
  , _segmentDuration           :: Seconds  -- ^ non-negative
  } deriving Show

makeLenses ''Segment

instance Audio Segment where
  load filePath = do
    duration     <- Sox.getDuration     filePath
    channelCount <- Sox.getChannelCount filePath
    sampleRate   <- Sox.getSampleRate   filePath
    pure $ Segment [filePath] duration channelCount sampleRate 0 duration
  toSox (Segment {..}) = Sox.files (_segmentSource ++ silentSource)
                       & addSilenceIfNeeded
                       & Sox.filter trim
                       & Sox.filter collapseChannels
    where
      -- must have the same sample rate as the source
      silentSource :: Source
      silentSource = ["--rate", show _segmentSourceSampleRate, "--null"]

      addSilenceIfNeeded :: Sox -> Sox
      addSilenceIfNeeded | needsSilence = Sox.filter addSilence
                         | otherwise    = id
        where
          silenceDuration :: Seconds
          silenceDuration = max 0 (negate _segmentOffset)

          needsSilence :: Bool
          needsSilence = silenceDuration > 0

          addSilence :: Filter
          addSilence = "delay"
                     : replicate _segmentSourceChannelCount (show silenceDuration)

      trim :: Filter
      trim = ["trim", show trimDuration, show _segmentDuration]
        where
          trimDuration :: Seconds
          trimDuration = max 0 _segmentOffset

      -- otherwise the silentSource will be saved as an extra channel
      collapseChannels :: Filter
      collapseChannels
        = [ "remix"
          , "-m"  -- "manual" volumes, otherwise it gets reduced
          , intercalate "," (map show leftChannels)
          , intercalate "," (map show rightChannels)
          ]
        where
          channelCount :: ChannelCount
          channelCount = 1 + _segmentSourceChannelCount

          leftChannels  = [1,3..channelCount]
          rightChannels = [2,4..channelCount]
