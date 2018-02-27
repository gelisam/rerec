{-# LANGUAGE RecordWildCards, TemplateHaskell, TypeFamilies #-}
module ReRec.AudioVector where

import Control.Lens
import Data.Semigroup

import Camera
import ReRec.Audio
import ReRec.AudioFile
import ReRec.ChannelMap
import ReRec.Segment
import ReRec.Types
import qualified ReRec.Sox as Sox


-- |
-- A variant of 'Segment' which supports more combinators. This time the
-- duration can be negative; this gets truncated to zero when playing or
-- saving, but is useful to express the following idiom for overlapping the
-- end of one clip with the beginning of another:
--
-- > :set NegativeLiterals
-- > clip1 <> silence -1 <> clip2
--
-- Denotationally, there is an infinite amount of audio data (most of which is
-- silence), and the duration vector selects part of it.
data AudioVector = AudioVector
  { _audioVectorSegments :: [Segment]
  , _audioVectorDuration :: Seconds
  } deriving Show

makeLenses ''AudioVector

-- |
-- 'followedBy' and 'overlap' are both monoidal; we use 'followedBy'.
instance Monoid AudioVector where
  mempty = silence 0
  mappend = followedBy

instance Semigroup AudioVector

instance Camera AudioVector where
  type Vector AudioVector = Seconds
  moveWorld = over (audioVectorSegments . each)
            . moveWorld

instance Audio AudioVector where
  load filePath = do
    segment <- load filePath
    let duration = view segmentDuration segment
    pure $ AudioVector [segment] duration
  toSox (AudioVector {..}) = Sox source filter_
    where
      audioFiles :: [AudioFile]
      audioFiles = map _segmentFile _audioVectorSegments

      source :: Sox.Source
      source = foldr (<>)
                     (silentSourceMatchingAudioFiles audioFiles)  -- last
                     (audioFileSource <$> audioFiles)

      filter_ :: Sox.Filter
      filter_ = Sox.delayFilter adjustedOffsets  -- for positive offsets
             <> Sox.trimFilter offset duration   -- for negative offsets
             <> Sox.remixFilter remixChannels

      channelOffsets :: ChannelMap Seconds
      channelOffsets = segmentsOffsets _audioVectorSegments
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

      duration :: Seconds
      duration = max 0 _audioVectorDuration

      remixChannels :: ChannelMap Channel
      remixChannels = segmentsRemixChannels _audioVectorSegments
                   <> silentSourceRemixChannels


silence :: Seconds -> AudioVector
silence dt = AudioVector [] dt

-- |
-- Play one clip after the other.
--
-- Denotationally: align the second audio data so the second vector starts
-- where the first vector ends, add the audio data pointwise, then add the
-- vectors.
followedBy
  :: AudioVector -> AudioVector -> AudioVector
followedBy v1 v2 = AudioVector segments' duration'
  where
    segments1 :: [Segment]
    segments1 = view audioVectorSegments v1

    segments2 :: [Segment]
    segments2 = view audioVectorSegments (moveWorld duration1 v2)

    segments' :: [Segment]
    segments' = segments1 ++ segments2

    duration1 :: Seconds
    duration1 = view audioVectorDuration v1

    duration2 :: Seconds
    duration2 = view audioVectorDuration v2

    duration' :: Seconds
    duration' = duration1 + duration2

-- |
-- Play both clips at the same time.
--
-- Denotationally: align the second audio data so the vector start at the same
-- point, add the audio data pointwise, then keep the most positive vector.
overlap :: AudioVector -> AudioVector -> AudioVector
overlap v1 v2 = AudioVector segments' duration'
  where
    segments1 :: [Segment]
    segments1 = view audioVectorSegments v1

    segments2 :: [Segment]
    segments2 = view audioVectorSegments v2

    segments' :: [Segment]
    segments' = segments1 ++ segments2

    duration1 :: Seconds
    duration1 = view audioVectorDuration v1

    duration2 :: Seconds
    duration2 = view audioVectorDuration v2

    duration' :: Seconds
    duration' = max duration1 duration2


silentSourceMatchingAudioFiles :: [AudioFile] -> Sox.Source
silentSourceMatchingAudioFiles [] = Sox.silentSource
silentSourceMatchingAudioFiles (x:_) = silentSourceMatchingAudioFile x

segmentsOffsets :: [Segment] -> ChannelMap Seconds
segmentsOffsets = foldMap segmentOffsets

segmentsRemixChannels :: [Segment] -> ChannelMap Channel
segmentsRemixChannels = foldMap segmentRemixChannels
