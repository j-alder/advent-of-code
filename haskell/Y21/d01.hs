module Y21.D03 where

import Data.Text qualified as T
import Data.Text (Text)
import Data.Text.IO qualified as TIO


parseInput :: Text -> [Int]
parseInput raw =
  map (read . T.unpack) . init $ T.split (== '\n') raw

soln1 :: [Int] -> Int
soln1 ns = 
  uncurry (*) $ head [(x, y) | x <- ns, y <- ns, x /= y && x + y == 2020]

prd :: (Int, Int, Int) -> Int
prd (a, b, c) = a * b * c

soln2 :: [Int] -> Int
soln2 ns = 
  prd $ head [(x, y, z) | x <- ns, y <- ns, z <- ns, x /= y && y /= z && x + y + z == 2020]

main :: IO ()
main = do
  raw <- TIO.readFile "../../input/y2021/d01.txt"
  let input = parseInput raw
      ans1 = soln1 input
      ans2 = soln2 input
  print ans1
  print ans2
