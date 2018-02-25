{-# LANGUAGE GeneralizedNewtypeDeriving, LambdaCase, RecordWildCards, TemplateHaskell, ViewPatterns #-}
-- |
-- This module is intended to be imported qualified.
module ReRec.Sox where

import Control.Concurrent.Async
import Data.Foldable
import Data.List
import Data.Semigroup

import System.Process.Async

import ReRec.ChannelMap (ChannelMap(..))
import ReRec.Types
import qualified ReRec.ChannelMap as ChannelMap


newtype Source = Source
  { unSource :: [String]
  } deriving Show

newtype Destination = Destination
  { unDestination :: [String]
  } deriving Show

newtype Filter = Filter
  { unFilter :: [String]
  } deriving (Monoid, Semigroup, Show)


instance Semigroup Source where
  Source xs1 <> Source xs2 = Source
                           $ "-M"  -- 1 (or 2) channel per source
                           : dropWhile (== "-M") xs1
                          <> dropWhile (== "-M") xs2


fileSource
  :: FilePath -> Source
fileSource filePath = Source [filePath]

microphoneSource
  :: Source
microphoneSource = Source ["--default-device"]

silentSource
  :: Source
silentSource = Source ["--null"]

silentSourceMatchingSampleRate
  :: Hz -> Source
silentSourceMatchingSampleRate rate = Source ["--rate", show rate, "--null"]


fileDestination
  :: FilePath -> Destination
fileDestination filePath = Destination [filePath]

speakersDestination
  :: Destination
speakersDestination = Destination ["--default-device"]


delayFilter
  :: ChannelMap Seconds -> Filter
delayFilter (ChannelMap.null -> True) = Filter []
delayFilter delays = Filter $ "delay" : map show (unChannelMap delays)

remixFilter
  :: ChannelMap Channel -> Filter
remixFilter (ChannelMap.null -> True) = Filter []
remixFilter destinations = Filter
                         $ "remix"
                         : "-m"  -- "manual" volumes, otherwise it gets reduced
                         : map (encode . remix) [1..lastChannel]
  where
    lastChannel :: Channel
    lastChannel = maximum . toList $ destinations

    remix :: Channel -> [Channel]
    remix destination = map fst
                      . filter ((== destination) . snd)
                      . ChannelMap.toList
                      $ destinations

    encode :: [Channel] -> String
    encode [] = "0"
    encode xs = intercalate "," . map show $ xs

reverseFilter
  :: Filter
reverseFilter
  = Filter ["reverse"]

silenceFilter
  :: Seconds -> Double -> Filter
silenceFilter duration threshold
  = Filter ["silence", "1", show duration, show threshold]

trimFilter
  :: Seconds -> Seconds -> Filter
trimFilter offset duration
  = Filter ["trim", show offset, show duration]


run
  :: Source -> Destination -> Filter -> IO (Async ())
run source destination filter_ = execute "sox" args
  where
    args = unSource source
        <> unDestination destination
        <> unFilter filter_

run_
  :: Source -> Destination -> Filter -> IO ()
run_ source destination filter_ = do
  thread <- run source destination filter_
  wait thread
