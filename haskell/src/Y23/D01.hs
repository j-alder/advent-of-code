module Y23 where

import Data.Time
import Util (readInput, splitStr)

isDigit :: Char -> Bool
isDigit c = c >= '0' && c <= '9'

digits :: String -> String -> String
digits str res
  | null str = res
  | isDigit (head str) = digits (tail str) (res ++ [head str])
  | otherwise = digits (tail str) res

getNum :: String -> Int
getNum str =
  if length d > 0 then read ([head d] ++ [last d]) else 0
  where
    d = digits str ""

soln1 :: IO ()
soln1 = do
  start <- getCurrentTime
  input <- readInput "2023" "1"
  let inputSplit = map (\s -> getNum s) (splitStr '\n' input)
  let x = foldr (\s a -> a + s) 0 inputSplit
  stop <- getCurrentTime
  print $ diffUTCTime stop start
  putStrLn (show x)

