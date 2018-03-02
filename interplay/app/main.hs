module Main where

import System.Environment

import Exe.Interplay

main :: IO ()
main = interplay =<< getArgs
