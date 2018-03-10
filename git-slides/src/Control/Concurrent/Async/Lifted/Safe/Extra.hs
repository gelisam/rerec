{-# LANGUAGE FlexibleContexts, LambdaCase #-}
module Control.Concurrent.Async.Lifted.Safe.Extra where

import Control.Concurrent.Async.Lifted.Safe
import Control.Monad.Trans.Control
import Data.List


raceMaybes
  :: (Forall (Pure m), MonadBaseControl IO m)
  => [Async (Maybe a)] -> m (Maybe a)
raceMaybes as = waitAny as >>= \case
  (_, Just x) -> pure (Just x)
  (a, Nothing) -> raceMaybes (delete a as)
