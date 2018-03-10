{-# LANGUAGE FlexibleContexts, LambdaCase, ScopedTypeVariables #-}
module Lib.GitSlides
  ( Slide
  , currentSlide, prevSlide, nextSlide, lastSlide
  , fileAtSlide
  ) where

import Lib.GitSlides.File
import Lib.GitSlides.Navigation
import Lib.GitSlides.Types
