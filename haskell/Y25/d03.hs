module Y25.D03 where

import Data.List (sort)
import Data.Char (ord)


jolts :: Char -> (Maybe Char, Maybe Char) -> (Maybe Char, Maybe Char)
jolts c (Nothing, Nothing) = (Nothing, Just c)
jolts c (Nothing, Just m) = (Just c, Just m)
jolts c (Just n, Just m) 
  | c > n = (Just c, if n > m then Just n else Just m)
  | otherwise = (Just n, Just m)

maxJolts :: [Char] -> Int
maxJolts = toInt . foldr jolts (Nothing, Nothing)
  where 
    toInt (Nothing, Nothing) = 0
    toInt (Nothing, Just m) = read [m]
    toInt (Just n, Just m) = read [n, m]

-- solution using right-associated fold, not quite correct yet
soln1 :: [String] -> [Int] -> Int
soln1 [] acc = sum acc
soln1 (x : xs) acc = soln1 xs $ maxJolts x : acc

main :: IO ()
main = do
  input <- readFile "../../../input/y2025/d03.txt"
  let lns = lines input
      s1 = soln1 lns []
  print s1
    