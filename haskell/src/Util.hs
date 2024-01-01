module Util where

fmtDay :: String -> String
fmtDay d =
  if length d == 1
    then "0" ++ d
    else d

fmtPath :: String -> String -> String
fmtPath year day =
    "../input/y" ++ year ++ "/d" ++ (fmtDay day) ++ ".txt"

fmtSolnStr :: (String, String) -> String
fmtSolnStr ans = "Part 1: " ++ fst ans ++ "\nPart 2: " ++ snd ans ++ "\n"

readInput :: String -> String -> IO String
readInput year day = readFile (fmtPath year day)

splitStr :: Char -> String -> [String]
splitStr c str = if null n'' then [n']
    else n' : splitStr c (tail n'')
    where
        n = span (/= c) str
        n' = fst n
        n'' = snd n

