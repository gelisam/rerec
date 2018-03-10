{-# LANGUAGE FlexibleContexts, LambdaCase, ScopedTypeVariables #-}
module Lib.GitSlides.Navigation where

import Control.Concurrent.Async.Lifted.Safe
import Control.Monad.Trans.Control
import Data.List.NonEmpty (NonEmpty((:|)))
import Git.Reference
import Git.Types

import Control.Concurrent.Async.Lifted.Safe.Extra
import Git.Commit.Extra
import Lib.GitSlides.Types
import Lib.GitSlides.Exploration


-- partial if a branch doesn't resolve to a commit.
-- Nothing if the HEAD cannot be found from any of the branches.
-- non-deterministic if multiple branches descend from HEAD.
currentSlide
  :: forall r m. (Forall (Pure m), MonadBaseControl IO m, MonadGit r m)
  => m (Maybe (Slide r))
currentSlide = do
  currentCommit >>= \case
    Just commit -> do
      branches <- listReferences
      verifiers <- traverse (async . flip exploreBranchUntil commit) branches
      raceMaybes verifiers
    Nothing -> pure Nothing

prevSlide
  :: MonadGit r m
  => Slide r -> m (Maybe (Slide r))
prevSlide (commit :| commits) = do
  lookupCommitParents commit >>= \case
    [parentCommit] -> pure $ Just (parentCommit :| commit : commits)
    _ -> pure Nothing  -- merge or initial commit, stop searching

nextSlide
  :: Slide r -> Maybe (Slide r)
nextSlide (_ :| [])               = Nothing
nextSlide (_ :| commit : commits) = Just (commit :| commits)

-- partial if the branch doesn't resolve to a commit.
lastSlide
  :: MonadGit r m
  => RefName -> m (Slide r)
lastSlide branch = do
  commit <- resolveReferenceToCommit branch
  pure (commit :| [])
