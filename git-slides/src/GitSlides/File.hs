module GitSlides.File where

import Control.Monad
import Data.Text.Encoding
import Git.Blob
import Git.Tree
import Git.Types
import System.FilePath
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.Text as Text

import GitSlides.Types


fileAtBlobOid
  :: MonadGit r m
  => FilePath -> BlobOid r -> m (Maybe Lazy.ByteString)
fileAtBlobOid filePath blobOid_ = do
  case splitDirectories filePath of
    []    -> Just <$> catBlobLazy blobOid_
    ["."] -> Just <$> catBlobLazy blobOid_
    _     -> pure Nothing

fileAtTreeEntry
  :: MonadGit r m
  => FilePath -> TreeEntry r -> m (Maybe Lazy.ByteString)
fileAtTreeEntry filePath (BlobEntry blobOid_ _)   = fileAtBlobOid   filePath blobOid_
fileAtTreeEntry filePath (TreeEntry treeOid_)     = fileAtTreeOid   filePath treeOid_
fileAtTreeEntry filePath (CommitEntry commitOid_) = fileAtCommitOid filePath commitOid_

fileAtTree
  :: MonadGit r m
  => FilePath -> Tree r -> m (Maybe Lazy.ByteString)
fileAtTree filePath tree = do
  case splitDirectories filePath of
    fileSegment:fileSegments -> do
      let treeFilePath = encodeUtf8 . Text.pack $ fileSegment
      treeEntries <- listTreeEntries tree
      case lookup treeFilePath treeEntries of
        Just treeEntry_ -> fileAtTreeEntry (joinPath fileSegments) treeEntry_
        Nothing -> pure Nothing
    _ -> pure Nothing

fileAtTreeOid
  :: MonadGit r m
  => FilePath -> TreeOid r -> m (Maybe Lazy.ByteString)
fileAtTreeOid filePath = lookupTree >=> fileAtTree filePath

fileAtCommit
  :: MonadGit r m
  => FilePath -> Commit r -> m (Maybe Lazy.ByteString)
fileAtCommit filePath = fileAtTreeOid filePath . commitTree

fileAtCommitOid
  :: MonadGit r m
  => FilePath -> CommitOid r -> m (Maybe Lazy.ByteString)
fileAtCommitOid filePath = lookupCommit >=> fileAtCommit filePath

fileAtSlide
  :: MonadGit r m
  => FilePath -> Slide r -> m (Maybe Lazy.ByteString)
fileAtSlide filePath = fileAtCommit filePath . NonEmpty.head
