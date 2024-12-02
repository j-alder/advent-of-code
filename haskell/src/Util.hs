module Util where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Time (diffUTCTime, getCurrentTime, UTCTime)

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
readInput :: String -> String -> IO Text
readInput year day = TIO.readFile (fmtInputPath year day)

---- answer formatting ----

-- | Format a solution string for a given part, answer, and execution time.

fmtSoln :: String -> Text -> Double -> String
fmtSoln part ans diff = part ++ ": " ++ show ans ++ "\ntook: " ++ show diff ++ "ms"

diffInMillis :: UTCTime -> UTCTime -> Double
diffInMillis start end = (realToFrac (diffUTCTime end start) :: Double) * 1000

printAnswersWithTime :: (Text, UTCTime, UTCTime) -> (Text, UTCTime, UTCTime) -> IO ()
printAnswersWithTime ans1 ans2 = do
  putStrLn (fmtSoln "Part 1" (extractFirst ans1) (diffInMillis (extractSecond ans1) (extractThird ans1)))
  putStrLn (fmtSoln "Part 2" (extractFirst ans2) (diffInMillis (extractSecond ans2) (extractThird ans2)))

fmtSolnWithRuntime :: (Text -> Text) -> (Text-> Text) -> IO ()
fmtSolnWithRuntime partOne partTwo = do
  startTime1 <- getCurrentTime
  let ans1 = partOne
  endTime1 <- getCurrentTime
  startTime2 <- getCurrentTime
  let ans2 = partTwo
  endTime2 <- getCurrentTime
  printAnswersWithTime (ans1, startTime1, endTime1) (ans2, startTime2, endTime2)

---- solution utils ----

extractFirst :: (a, b, c) -> a
extractFirst (a, _, _) = a

extractSecond :: (a, b, c) -> b
extractSecond (_, b, _) = b

extractThird :: (a, b, c) -> c
extractThird (_, _, c) = c

-- | Split a string into a list of strings, separated by the given character.
splitByChar :: Char -> Text -> [Text]
splitByChar c = T.splitOn (T.singleton c)
