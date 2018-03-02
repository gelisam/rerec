{-# LANGUAGE RecordWildCards, TemplateHaskell, TypeFamilies, ViewPatterns #-}
module Lib.Interplay.Segment where

import Control.Lens
import Data.Semigroup

import Camera
import Lib.Interplay.Audio
import Lib.Interplay.AudioFile
import Lib.Interplay.ChannelMap (ChannelMap(..))
import Lib.Interplay.Types
import qualified Lib.Interplay.ChannelMap as ChannelMap
import qualified Lib.Interplay.Sox as Sox


-- |
-- A portion of an audio clip, except that the selected segment need not be
-- completely included within said clip. If the offset is positive or the
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
  moveWorld dx = over segmentOffset (+ dx)

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
      filter_ = Sox.delayFilter adjustedOffsets         -- for positive offset
             <> Sox.trimFilter offset _segmentDuration  -- for negative offset
             <> Sox.remixFilter remixChannels

      channelOffsets :: ChannelMap Seconds
      channelOffsets = segmentOffsets segment
                    <> silentSourceOffsets

      mostNegativeOffset :: Seconds
      mostNegativeOffset = minimum
                         . (0:)
                         . filter (< 0)
                         . unChannelMap
                         $ channelOffsets

      -- make all the offsets non-negative
      adjustedOffsets :: ChannelMap Seconds
      adjustedOffsets = (+ negate mostNegativeOffset) <$> channelOffsets

      offset :: Seconds
      offset = negate mostNegativeOffset

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
    offset = _segmentOffset

silentSourceOffsets :: ChannelMap Seconds
silentSourceOffsets = ChannelMap.eachChannel 1 $ \_ -> 0

segmentRemixChannels :: Segment -> ChannelMap Channel
segmentRemixChannels (Segment {..}) = ChannelMap.eachChannel channelCount id
  where
    channelCount :: ChannelCount
    channelCount = view audioFileChannelCount _segmentFile

silentSourceRemixChannels :: ChannelMap Channel
silentSourceRemixChannels = ChannelMap.eachChannel 1 id
