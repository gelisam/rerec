{-# LANGUAGE FlexibleContexts, LambdaCase, ScopedTypeVariables #-}
module GitSlides
  ( Slide
  , currentSlide, prevSlide, nextSlide, lastSlide
  , fileAtSlide, overwriteAtSlide
  ) where

import GitSlides.File.Read
import GitSlides.File.Write
import GitSlides.Navigation
import GitSlides.Types
