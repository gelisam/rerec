{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveTraversable #-}
-- |
-- This module is intended to be imported half-qualified, like "Data.Map":
--
-- > import Lib.Interplay.ChannelMap (ChannelMap(..))
-- > import qualified Lib.Interplay.ChannelMap as ChannelMap
module Lib.Interplay.ChannelMap where

import Data.Semigroup

import Lib.Interplay.Types


newtype ChannelMap a = ChannelMap
  { unChannelMap :: [a]
  } deriving (Foldable, Functor, Monoid, Semigroup,Show, Traversable)

eachChannel :: ChannelCount -> (Channel -> a) -> ChannelMap a
eachChannel channelCount f = ChannelMap $ map f [1..channelCount]

findWithDefault :: a -> Channel -> ChannelMap a -> a
findWithDefault def i0 = go i0 . unChannelMap
  where
    go _ [] = def
    go 1 (x:_) = x
    go i (_:xs) = go i xs

keys :: ChannelMap a -> [Channel]
keys = zipWith const [1..] . unChannelMap

null :: ChannelMap a -> Bool
null = Prelude.null . unChannelMap

toList :: ChannelMap a -> [(Channel, a)]
toList = zip [1..] . unChannelMap
