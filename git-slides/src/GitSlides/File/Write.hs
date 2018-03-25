module GitSlides.File.Write where

import Control.Lens
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Data.List.NonEmpty (NonEmpty(..))
import Data.Text.Encoding
import Data.Traversable
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

overwriteAtSlideHelper
  :: MonadGit r m
  => Maybe RefName -> FilePath -> Lazy.ByteString -> Slide r -> m (Slide r)
overwriteAtSlideHelper branchMay filePath contents (commit0 :| commits1Z) = do
  commit0' <- overwriteAtCommit filePath contents commit0

  -- we are changing the history, so all the later slides need to be rewritten
  commits1Z' <- flip evalStateT commit0' $ do
    for commits1Z $ \commitB -> do
      commitA <- get
      commitB' <- lift $ createCommit [commitOid commitA]
                                      (commitTree commitB)
                                      (commitAuthor commitB)
                                      (commitCommitter commitB)
                                      (commitLog commitB)
                                      branchMay
      put commitB'
      pure commitB'
  pure (commit0' :| commits1Z')

overwriteAtSlide
  :: MonadGit r m
  => FilePath -> Lazy.ByteString -> Slide r -> m (Slide r)
overwriteAtSlide = overwriteAtSlideHelper Nothing

overwriteAtSlideshow
  :: MonadGit r m
  => FilePath -> Lazy.ByteString -> Slideshow r -> m (Slideshow r)
overwriteAtSlideshow filePath contents slideshow = do
  let branch = view slideshowBranch slideshow
  forOf slideshowCurrentSlide slideshow $ \slide -> do
    overwriteAtSlideHelper (Just branch) filePath contents slide
