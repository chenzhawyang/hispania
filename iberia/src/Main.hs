module Main (main) where

import Prelude hiding (putStr, putStrLn, length)

import Data.ByteString.Char8 (putStr, putStrLn)
import Data.ByteString.UTF8

main :: IO ()
main = do
  putStrLn $ fromString "pʰ"
