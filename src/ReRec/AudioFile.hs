{-# LANGUAGE TemplateHaskell #-}
module ReRec.AudioFile where

import Control.Lens

import System.Process.Extra

import ReRec.Audio
import ReRec.Types
import qualified ReRec.Sox as Sox


data AudioFile = AudioFile
  { _audioFilePath         :: FilePath
  , _audioFileChannelCount :: ChannelCount
  , _audioFileDuration     :: Seconds
  , _audioFileSampleRate   :: Hz
  } deriving Show

makeLenses ''AudioFile

instance Audio AudioFile where
  load filePath = AudioFile filePath
              <$> soxi "-c"
              <*> soxi "-D"
              <*> soxi "-r"
    where
      soxi :: Read a => String -> IO a
      soxi flag = readProcessLn "soxi" [flag, filePath]
  toSox audioFile = Sox (audioFileSource audioFile) mempty

audioFileSource :: AudioFile -> Sox.Source
audioFileSource = Sox.fileSource . view audioFilePath
