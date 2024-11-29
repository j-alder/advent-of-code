module Util where

import Data.Time

---- setup utils ----

-- | Format a day string to ensure it is two digits.
fmtDay :: String -> String
fmtDay d =
  if length d == 1
    then "0" ++ d
    else d

-- | Format the path for the input file for a given year and day.
fmtInputPath :: String -> String -> String
fmtInputPath year day =
    "../input/y" ++ year ++ "/d" ++ fmtDay day ++ ".txt"

-- | Read the input string for a given year and day.
readInput :: String -> String -> IO String
readInput year day = readFile (fmtInputPath year day)

---- answer formatting ----

fmtSolnStrPartOne :: String -> Double -> String
fmtSolnStrPartOne = fmtSolnStr "Part 1"

fmtSolnStrPartTwo :: String -> Double -> String
fmtSolnStrPartTwo = fmtSolnStr "Part 2"

-- | Format a solution string for a given part, answer, and execution time.
fmtSolnStr :: String -> String -> Double -> String
fmtSolnStr part ans diff = part ++ ": " ++ ans ++ "\ntook: " ++ show diff ++ "ms"

diffInMillis :: UTCTime -> UTCTime -> Double
diffInMillis start end = (realToFrac (diffUTCTime end start) :: Double) * 1000

printAnswerOneWithTime :: String -> UTCTime -> UTCTime -> IO ()
printAnswerOneWithTime ans startTime endTime = 
  putStrLn (fmtSolnStrPartOne ans (diffInMillis startTime endTime))

printAnswerTwoWithTime :: String -> UTCTime -> UTCTime -> IO ()
printAnswerTwoWithTime ans startTime endTime = 
  putStrLn (fmtSolnStrPartTwo ans (diffInMillis startTime endTime))

---- solution utils ----

-- | Split a string into a list of strings, separated by the given character.
splitStr :: Char -> String -> [String]
splitStr c str = if null n'' then [n']
    else n' : splitStr c (tail n'')
    where
        n = span (/= c) str
        n' = fst n
        n'' = snd n
