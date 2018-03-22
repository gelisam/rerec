{-# LANGUAGE FlexibleContexts, LambdaCase, ScopedTypeVariables #-}
module GitSlides
  ( Slide
  , currentSlide, prevSlide, nextSlide, lastSlide
  , fileAtSlide
  ) where

import GitSlides.File.Read
import GitSlides.Navigation
import GitSlides.Types
