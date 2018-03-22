{-# LANGUAGE ViewPatterns #-}
module Exe.Interplay where

import System.Directory
import System.Environment
import System.Exit
import Text.Printf
import Text.Read

import Interplay


parseAudioVector :: String -> IO (Maybe AudioVector)
parseAudioVector (readMaybe -> Just delay) = pure . Just . silence $ delay
parseAudioVector filePath = do
  fileExists <- doesFileExist filePath
  if fileExists
  then Just <$> load filePath
  else pure $ Nothing


printUsage :: IO ()
printUsage = do
  progName <- getProgName
  putStrLn "usage:"
  printf   "  %s [FILE | DELAY]\n" progName
  putStrLn ""
  putStrLn "Plays each of the audio FILEs one after the other."
  putStrLn "DELAY pauses for the given number of seconds."
  putStrLn "Use negative delays to overlay the end of one FILE on the beginning of the next!"

interplay :: [String] -> IO ()
interplay args = do
  audioVectorMay <- mconcat <$> traverse parseAudioVector args
  case audioVectorMay of
    Nothing -> do
      printUsage
      exitFailure
    Just audioVector -> do
      play_ audioVector
