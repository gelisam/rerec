{-# LANGUAGE FlexibleContexts, LambdaCase, ScopedTypeVariables #-}
module GitSlides.Navigation where

import Control.Concurrent.Async.Lifted.Safe
import Control.Lens
import Control.Monad.Trans.Control
import Control.Monad.Trans.Maybe
import Data.List.NonEmpty (NonEmpty((:|)))
import Git.Reference
import Git.Types

import Control.Concurrent.Async.Lifted.Safe.Extra
import Git.Commit.Extra
import GitSlides.Types
import GitSlides.Exploration


-- partial if a branch doesn't resolve to a commit.
-- Nothing if the HEAD cannot be found from any of the branches.
-- non-deterministic if multiple branches descend from HEAD.
currentSlide
  :: forall r m. (Forall (Pure m), MonadBaseControl IO m, MonadGit r m)
  => m (Maybe (Slide r))
currentSlide = runMaybeT
             . fmap (view slideshowCurrentSlide)
             . MaybeT
             $ currentSlideshow

-- partial if a branch doesn't resolve to a commit.
-- Nothing if the HEAD cannot be found from any of the branches.
-- non-deterministic if multiple branches descend from HEAD.
currentSlideshow
  :: forall r m. (Forall (Pure m), MonadBaseControl IO m, MonadGit r m)
  => m (Maybe (Slideshow r))
currentSlideshow = do
  currentCommit >>= \case
    Just commit -> do
      branches <- listReferences
      verifiers <- traverse (async . flip exploreBranchUntil commit) branches
      raceMaybes verifiers
    Nothing -> pure Nothing


firstSlide
  :: MonadGit r m
  => Slide r -> m (Slide r)
firstSlide slide = prevSlide slide >>= \case
  Nothing     -> pure slide
  Just slide' -> firstSlide slide'

firstSlideshow
  :: MonadGit r m
  => Slideshow r -> m (Slideshow r)
firstSlideshow = traverseOf slideshowCurrentSlide firstSlide


prevSlide
  :: MonadGit r m
  => Slide r -> m (Maybe (Slide r))
prevSlide (commit :| commits) = do
  lookupCommitParents commit >>= \case
    [parentCommit] -> pure $ Just (parentCommit :| commit : commits)
    _ -> pure Nothing  -- merge or initial commit, stop searching

prevSlideshow
  :: MonadGit r m
  => Slideshow r -> m (Maybe (Slideshow r))
prevSlideshow = runMaybeT
              . traverseOf slideshowCurrentSlide (MaybeT . prevSlide)


nextSlide
  :: Slide r -> Maybe (Slide r)
nextSlide (_ :| [])               = Nothing
nextSlide (_ :| commit : commits) = Just (commit :| commits)

nextSlideshow
  :: Slideshow r -> Maybe (Slideshow r)
nextSlideshow = runIdentity
              . runMaybeT
              . traverseOf slideshowCurrentSlide (MaybeT . Identity . nextSlide)


lastSlide
  :: Slide r -> Slide r
lastSlide (_ :| commit : commits) = lastSlide (commit :| commits)
lastSlide commits = commits

lastSlideshow
  :: Slideshow r -> Slideshow r
lastSlideshow = over slideshowCurrentSlide lastSlide
