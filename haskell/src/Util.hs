module Util where

parseFile path = do
  fileStr <- readFile path
  return $ lines fileStr

parseFile' path =
  readFile path
    >>= lines
