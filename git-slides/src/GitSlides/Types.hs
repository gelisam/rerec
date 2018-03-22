module GitSlides.Types where

import Data.List.NonEmpty
import Git.Types


-- the current commit and all the commits which follow.
type Slide r = NonEmpty (Commit r)
