{-# LANGUAGE RecordWildCards #-}
module ReRec.AutoTrim where

import Control.Lens
import Data.Monoid

import ReRec.Audio
import ReRec.Types
import qualified ReRec.Sox as Sox


newtype AutoTrim a = AutoTrim
  { unAutoTrim :: a
  } deriving Show

instance Audio a => Audio (AutoTrim a) where
  load = fmap AutoTrim
       . load
  toSox = over soxFilter (<> autoTrimFilter)
        . toSox
        . unAutoTrim


-- @man sox@ recommends "voc reverse voc reverse", but when I tried that
-- it removed the end of my last word.
autoTrimFilter :: Sox.Filter
autoTrimFilter = Sox.silenceFilter duration threshold
              <> Sox.reverseFilter
              <> Sox.silenceFilter duration threshold
              <> Sox.reverseFilter
  where
    duration :: Seconds
    duration = 0.1

    threshold :: Double
    threshold = 0.1
