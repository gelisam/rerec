{-# LANGUAGE RecordWildCards, TemplateHaskell #-}
module ReRec.Sox where

import Control.Concurrent.Async
import Control.Lens

import System.Process.Async
import System.Process.Extra


type Seconds = Double
type Filter = [String]

data Sox = Sox
  { _soxSources
      :: [String]
  , _soxFilters
      :: [String]
  }
  deriving Show

makeLenses ''Sox


runSox
  :: Sox
  -> String  -- ^ destination, i.e. a FilePath or "--default-device"
  -> IO (Async ())
runSox (Sox {..}) destination = execute "sox" args
  where
    args = _soxSources ++ [destination] ++ _soxFilters

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

filter
  :: Filter -> Sox -> Sox
filter xs = over soxFilters (++ xs)


getDuration
  :: FilePath -> IO Seconds
getDuration filePath = readProcessLn "soxi" ["-D", filePath]

getChannelCount
  :: FilePath -> IO Int
getChannelCount filePath = readProcessLn "soxi" ["-c", filePath]
