module Y25.D03 where


import Data.Char
import Data.List
import Data.Maybe

type Bank = [Int]

slice :: [a] -> Int -> Int -> [a]
slice xs from to = take (to - from) (drop from xs)

maxJoltage :: Bank -> Int -> Int -> Int -> Int
maxJoltage bank since 0 result = result
maxJoltage bank since left result =
  let s = slice bank since (length bank - left + 1)
      maxSoFar = maximum s
      idx = fromJust $ elemIndex maxSoFar s
  in maxJoltage bank (since + idx + 1) (left - 1) (result * 10 + maxSoFar)

totalOutput :: Int -> [Bank] -> Int
totalOutput cellsCount banks =
  let poweredBanks = map (\bank -> maxJoltage bank 0 cellsCount 0) banks
   in sum poweredBanks

main :: IO ()
main = do
  content <- readFile "../../input/y2025/d03.txt"
  let
      banks = map (map digitToInt) $ lines content
      t1 = totalOutput 2 banks
      t2 = totalOutput 12 banks
   in do
        print t1
        print t2

