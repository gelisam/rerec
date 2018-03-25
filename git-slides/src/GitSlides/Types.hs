{-# LANGUAGE TemplateHaskell #-}
module GitSlides.Types where

import Control.Lens
import Data.List.NonEmpty
import Git.Types


-- the current commit and all the commits which follow.
type Slide r = NonEmpty (Commit r)

data Slideshow r = Slideshow
  { _slideshowBranch       :: RefName
  , _slideshowCurrentSlide :: Slide r
  }

makeLenses ''Slideshow
