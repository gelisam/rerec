{-# LANGUAGE LambdaCase #-}
module Lib.GitSlides.Navigation where

import Data.List.NonEmpty (NonEmpty((:|)))
import Git.Types

import Git.Commit.Extra
import Lib.GitSlides.Types


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
