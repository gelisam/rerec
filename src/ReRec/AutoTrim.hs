{-# LANGUAGE RecordWildCards #-}
module ReRec.AutoTrim where

import ReRec.Audio
import qualified ReRec.Sox as Sox


newtype AutoTrim a = AutoTrim
  { unAutoTrim :: a
  } deriving Show

instance Audio a => Audio (AutoTrim a) where
  load = fmap AutoTrim . load
  toSox = Sox.filter autoTrim . toSox . unAutoTrim


-- @man sox@ recommends "voc reverse voc reverse", but when I tried that
-- it removed the end of my last word.
autoTrim :: Sox.Filter
autoTrim = words "silence 1 0.1 0.1 reverse silence 1 0.1 0.1 reverse"
