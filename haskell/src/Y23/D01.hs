module Y23.D01 where

import Data.Time
import Util (readInput, splitStr, printAnswerWithTime)

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

solve :: String -> IO ()
solve input = do
  start <- getCurrentTime
  let inputSplit = map getNum (splitStr '\n' input)
  let ans = foldr (\s a -> a + s) 0 inputSplit
  stop <- getCurrentTime
  let diff = (realToFrac (diffUTCTime stop start) :: Double) * 1000
  printAnswerWithTime (show ans) diff
