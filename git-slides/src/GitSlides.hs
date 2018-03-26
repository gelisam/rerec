{-# LANGUAGE FlexibleContexts, LambdaCase, ScopedTypeVariables #-}
module GitSlides
  ( Slideshow
  , currentSlideshow, prevSlideshow, nextSlideshow, lastSlideshow
  , fileAtSlideshow, overwriteAtSlideshow
  ) where

import GitSlides.File.Read
import GitSlides.File.Write
import GitSlides.Navigation
import GitSlides.Types
