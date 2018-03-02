{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Lib.Interplay.Types where

import Control.Lens
import Text.Printf


-- | numbered from 1
type Channel = Int

-- | only 1 and 2 are supported
type ChannelCount = Int

type Hz = Int

newtype Seconds = Seconds
  { unSeconds :: Double
  } deriving (Eq, Fractional, Num, Ord)


instance Read Seconds where
  readsPrec p = over (each . _1) Seconds . readsPrec p

instance Show Seconds where
  show (Seconds 0) = "0"  -- without this, @max 0 (-1)@ would give "-0",
                          -- which sox interprets differently from "0"
  show (Seconds x) = printf "%f" x
