module GitSlides.File.Write where

import Control.Lens
import Data.List.NonEmpty (NonEmpty(..))
import Data.Text.Encoding
import Git.Tree.Builder
import Git.Types
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.Text as Text

import Git.Types.Extra
import GitSlides.Types


overwriteAtTreeT
  :: MonadGit r m
  => FilePath -> Lazy.ByteString -> m (TreeT r m ())
overwriteAtTreeT filePath contents = do
  let treeFilePath = encodeUtf8 . Text.pack $ filePath
  blobOid_ <- createBlob . BlobStringLazy $ contents
  pure $ putBlob treeFilePath blobOid_

overwriteAtTree
  :: MonadGit r m
  => FilePath -> Lazy.ByteString -> Tree r -> m (Tree r)
overwriteAtTree filePath contents tree = do
  treeT <- overwriteAtTreeT filePath contents
  treeOid_ <- mutateTree tree treeT
  lookupTree treeOid_

overwriteAtTreeOid
  :: MonadGit r m
  => FilePath -> Lazy.ByteString -> TreeOid r -> m (TreeOid r)
overwriteAtTreeOid filePath contents treeOid_ = do
  treeT <- overwriteAtTreeT filePath contents
  mutateTreeOid treeOid_ treeT

overwriteAtCommit
  :: MonadGit r m
  => FilePath -> Lazy.ByteString -> Commit r -> m (Commit r)
overwriteAtCommit filePath contents = traverseOf commitTreeL
                                    $ overwriteAtTreeOid filePath contents

overwriteAtSlide
  :: MonadGit r m
  => FilePath -> Lazy.ByteString -> Slide r -> m (Slide r)
overwriteAtSlide filePath contents (commit :| commits) = do
  commit' <- overwriteAtCommit filePath contents commit

  -- TODO: rebase commits on top of commit'
  pure (commit' :| commits)
