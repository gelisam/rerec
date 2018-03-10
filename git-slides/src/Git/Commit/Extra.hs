{-# LANGUAGE LambdaCase, OverloadedStrings #-}
module Git.Commit.Extra where

import Git.Reference
import Git.Types


-- Nothing if HEAD doesn't resolve to a commit (e.g. immediately after @git init@)
currentCommit
  :: MonadGit r m
  => m (Maybe (Commit r))
currentCommit = do
  resolveReference "HEAD" >>= \case
    Just oid -> lookupObject oid >>= \case
      CommitObj commit -> pure . Just $ commit
      _ -> pure Nothing
    _ -> pure Nothing

-- partial if the refName doesn't resolve to a commit.
resolveReferenceToCommit
  :: MonadGit r m
  => RefName -> m (Commit r)
resolveReferenceToCommit refName = do
  Just oid <- resolveReference refName
  CommitObj commit <- lookupObject oid
  pure commit
