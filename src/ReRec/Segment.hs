{-# LANGUAGE RecordWildCards, TemplateHaskell, TypeFamilies, ViewPatterns #-}
module ReRec.Segment where

import Control.Lens
import Data.Map (Map)
import Data.Semigroup
import qualified Data.Map as Map

import Camera
import ReRec.Audio
import ReRec.AudioFile
import ReRec.Types
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
  toSox (Segment {..}) = Sox source filter_
    where
      source :: Sox.Source
      source = audioFileSource _segmentFile
            <> silentSourceMatchingAudioFile _segmentFile

      filter_ :: Sox.Filter
      filter_ = offsetsFilter offsets
             <> trim
             <> collapseChannels

      offsets :: Map Channel Seconds
      offsets = Map.fromList
              . flip fmap channels $ \channel
             -> (channel, offset)
        where
          channelCount :: ChannelCount
          channelCount = view audioFileChannelCount _segmentFile

          channels :: [Channel]
          channels = [1..channelCount]

          offset :: Seconds
          offset = max 0 (negate _segmentOffset)

      trim :: Sox.Filter
      trim = Sox.trimFilter offset _segmentDuration
        where
          offset :: Seconds
          offset = max 0 _segmentOffset

      -- otherwise the silentSource will be saved as an extra channel
      collapseChannels :: Sox.Filter
      collapseChannels = Sox.remixFilter
                       . Map.fromList
                       $ flip fmap [1..sourceChannelCount] $ \channel
                      -> (channel, toTargetChannel channel)
        where
          sourceChannelCount :: ChannelCount
          sourceChannelCount = 1 + targetChannelCount

          targetChannelCount :: ChannelCount
          targetChannelCount = _audioFileChannelCount _segmentFile

          toTargetChannel :: Channel -> Channel
          toTargetChannel = (+ 1)
                          . (`mod` targetChannelCount)
                          . (subtract 1)

silentSourceMatchingAudioFile :: AudioFile -> Sox.Source
silentSourceMatchingAudioFile = Sox.silentSourceMatchingSampleRate
                              . view audioFileSampleRate

-- all the offsets must be non-negative
offsetsFilter :: Map Channel Seconds -> Sox.Filter
offsetsFilter (Map.null -> True) = mempty
offsetsFilter offsets = Sox.delayFilter
                      . Map.fromList
                      . flip fmap channels $ \channel
                     -> (channel, channelOffset channel)
  where
    channelCount :: ChannelCount
    channelCount = maximum . Map.keys $ offsets

    channels :: [Channel]
    channels = [1..channelCount]

    channelOffset :: Channel -> Seconds
    channelOffset channel = Map.findWithDefault 0 channel offsets
