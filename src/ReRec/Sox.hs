{-# LANGUAGE RecordWildCards, TemplateHaskell #-}
module ReRec.Sox where

import Control.Concurrent.Async
import Control.Lens

import System.Process.Async
import System.Process.Extra


type Seconds = Double
type Hz = Int
type ChannelCount = Int
type Source = [String]
type Destination = String -- a 'FilePath' or "--default-device"
type Filter = [String]

data Sox = Sox
  { _soxSource
      :: Source
  , _soxFilter
      :: Filter
  }
  deriving Show

makeLenses ''Sox


runSox
  :: Sox -> Destination -> IO (Async ())
runSox (Sox {..}) destination = execute "sox" args
  where
    args = _soxSource ++ [destination] ++ _soxFilter

runSox_
  :: Sox -> Destination -> IO ()
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
filter xs = over soxFilter (++ xs)


getDuration
  :: FilePath -> IO Seconds
getDuration filePath = readProcessLn "soxi" ["-D", filePath]

getChannelCount
  :: FilePath -> IO ChannelCount
getChannelCount filePath = readProcessLn "soxi" ["-c", filePath]

getSampleRate
  :: FilePath -> IO Hz
getSampleRate filePath = readProcessLn "soxi" ["-r", filePath]
