{-# LANGUAGE RecordWildCards, TemplateHaskell, TypeFamilies, ViewPatterns #-}
module ReRec.Segment where

import Control.Lens
import Data.Semigroup

import Camera
import ReRec.Audio
import ReRec.AudioFile
import ReRec.ChannelMap (ChannelMap)
import ReRec.Types
import qualified ReRec.ChannelMap as ChannelMap
import qualified ReRec.Sox as Sox


-- |
-- A portion of an audio clip, except that the selected segment need not be
-- completely included within said clip. If the offset is negative or the
-- duration extends beyond the clip's, the remainder will be filled with
-- silence.
data Segment = Segment
  { _segmentFile     :: AudioFile
  , _segmentOffset   :: Seconds
  , _segmentDuration :: Seconds  -- ^ non-negative
  } deriving Show

makeLenses ''Segment

instance Camera Segment where
  type Vector Segment = Seconds
  moveCamera dx = over segmentOffset (+ dx)

instance Audio Segment where
  load filePath = do
    audioFile <- load filePath
    let duration = view audioFileDuration audioFile
    pure $ Segment audioFile 0 duration
  toSox segment@(Segment {..}) = Sox source filter_
    where
      source :: Sox.Source
      source = audioFileSource _segmentFile
            <> silentSourceMatchingAudioFile _segmentFile

      filter_ :: Sox.Filter
      filter_ = Sox.delayFilter offsets
             <> Sox.trimFilter offset _segmentDuration
             <> Sox.remixFilter remixChannels

      offsets :: ChannelMap Seconds
      offsets = segmentOffsets segment
             <> silentSourceOffsets

      offset :: Seconds
      offset = max 0 _segmentOffset

      -- otherwise the silentSource will be saved as an extra channel
      remixChannels :: ChannelMap Channel
      remixChannels = segmentRemixChannels segment
                   <> silentSourceRemixChannels

silentSourceMatchingAudioFile :: AudioFile -> Sox.Source
silentSourceMatchingAudioFile = Sox.silentSourceMatchingSampleRate
                              . view audioFileSampleRate

segmentOffsets :: Segment -> ChannelMap Seconds
segmentOffsets (Segment {..}) = ChannelMap.eachChannel channelCount
                              $ \_ -> offset
  where
    channelCount :: ChannelCount
    channelCount = view audioFileChannelCount _segmentFile

    offset :: Seconds
    offset = max 0 (negate _segmentOffset)

silentSourceOffsets :: ChannelMap Seconds
silentSourceOffsets = ChannelMap.eachChannel 1 $ \_ -> 0

segmentRemixChannels :: Segment -> ChannelMap Channel
segmentRemixChannels (Segment {..}) = ChannelMap.eachChannel channelCount id
  where
    channelCount :: ChannelCount
    channelCount = view audioFileChannelCount _segmentFile

silentSourceRemixChannels :: ChannelMap Channel
silentSourceRemixChannels = ChannelMap.eachChannel 1 id
