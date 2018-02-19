{-# LANGUAGE RecordWildCards #-}
module ReRec.Sox where

import Control.Concurrent.Async

import System.Process.Async


data Sox = Sox
  { soxSources
      :: [String]
  , soxFilters
      :: [String]
  }
  deriving Show


runSox
  :: Sox
  -> String  -- ^ destination, i.e. a FilePath or "--default-device"
  -> IO (Async ())
runSox (Sox {..}) destination = execute "sox" args
  where
    args = soxSources ++ [destination] ++ soxFilters

runSox_
  :: Sox
  -> String  -- ^ destination, i.e. a FilePath or "--default-device"
  -> IO ()
runSox_ sox destination = do
  thread <- runSox sox destination
  wait thread


rec
  :: Sox
rec = Sox
  ["--default-device"]  -- the microphone
  []

file
  :: FilePath -> Sox
file filePath = Sox [filePath] []

files
  :: [FilePath] -> Sox
files filePaths = Sox ("-M":filePaths) []
