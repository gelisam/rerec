{-# LANGUAGE FlexibleContexts, TypeFamilies #-}
module Camera where

class Num (Vector a) => Camera a where
  type Vector a

  moveCamera
    :: Vector a -> a -> a
  moveCamera = moveWorld . negate

  moveWorld
    :: Vector a -> a -> a
  moveWorld = moveCamera . negate

  {-# MINIMAL moveCamera | moveWorld #-}
