module Y23.D01 where

import Data.Time ( diffUTCTime, getCurrentTime )
import Util ( readInput, splitStr, printAnswerWithTime )

isDigit :: Char -> Bool
isDigit c = c >= '0' && c <= '9'

digits :: String -> String -> String
digits str res
  | null str = res
  | isDigit (head str) = digits (tail str) (res ++ [head str])
  | otherwise = digits (tail str) res

getNum :: String -> Int
getNum str =
  if not (null d) then read (head d : [last d]) else 0
  where
    d = digits str ""

partOne :: [String] -> Int
partOne input = foldr (\s a -> a + s) 0 (map getNum input)

partTwo :: [String] -> Int
partTwo input = 0

solve :: String -> IO ()
solve input = do
  let inputList = splitStr '\n' input
  startOne <- getCurrentTime
  let ansOne = partOne inputList
  endOne <- getCurrentTime
  printAnswerWithTime (show ansOne) startOne endOne
  startTwo <- getCurrentTime
  let ansTwo = partTwo inputList
  endTwo <- getCurrentTime
  printAnswerWithTime (show ansTwo) startTwo endTwo
