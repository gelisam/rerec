module Git.Types.Extra where

import Control.Lens
import Git.Types


commitTreeL :: Lens' (Commit r) (TreeOid r)
commitTreeL f commit = (\treeOid_ -> commit { commitTree = treeOid_ })
                   <$> f (commitTree commit)
