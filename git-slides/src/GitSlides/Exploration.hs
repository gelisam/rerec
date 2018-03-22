{-# LANGUAGE LambdaCase #-}
module GitSlides.Exploration where

import Data.List.NonEmpty (NonEmpty((:|)))
import Git.Types

import Git.Commit.Extra
import GitSlides.Types


-- exploration stops if a merge commit is encountered, or otherwise at the initial commit.
-- Nothing if the ancestor is not found before then.
exploreSlidesUntil
  :: MonadGit r m
  => Slide r -> Commit r -> m (Maybe (Slide r))
exploreSlidesUntil halfZipper@(commit :| commits) ancestor
  | commitOid commit == commitOid ancestor = pure . Just $ halfZipper
  | otherwise = do
      lookupCommitParents commit >>= \case
        [parentCommit] -> exploreSlidesUntil (parentCommit :| commit : commits) ancestor
        _ -> pure Nothing  -- merge or initial commit, stop searching

-- partial if the branch doesn't resolve to a commit.
-- exploration stops if a merge commit is encountered, or otherwise at the initial commit.
-- Nothing if the ancestor is not found before then.
exploreBranchUntil
  :: MonadGit r m
  => RefName -> Commit r -> m (Maybe (Slide r))
exploreBranchUntil branch ancestor = do
  branchHead <- resolveReferenceToCommit branch
  exploreSlidesUntil (branchHead :| []) ancestor
