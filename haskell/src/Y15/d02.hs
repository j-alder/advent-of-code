module Y15.D02 where

import qualified Data.Text as T
import Data.Text (pack, unpack)
import Data.List (sort)

min' :: [Int] -> (Int, Int)
min' lst = 
  let lst' = sort lst
  in (head lst', lst' !! 1)

soln2 :: [[Int]] -> Int
soln2 presents = sumRibbon presents 0
  where
    sumRibbon :: [[Int]] -> Int -> Int
    sumRibbon [] s = s
    sumRibbon (p:ps) s = sumRibbon ps $ s + getSize p
    getSize :: [Int] -> Int
    getSize p =
      let m = min' p
      in uncurry (+) m + uncurry (+) m + product p

soln1 :: [[Int]] -> Int
soln1 presents = sumPaper presents 0
  where
    sumPaper :: [[Int]] -> Int -> Int
    sumPaper [] s = s
    sumPaper (p:ps) s = sumPaper ps $ s + getSize p
    getSize :: [Int] -> Int
    getSize p =
      let m = min' p
      in uncurry (*) m + sqFt p
    sqFt [l,w,h] = 2*l*w + 2*w*h + 2*h*l

main :: IO ()
main = do
  input <- readFile "../../input/y2015/d02.txt"
  let lns = T.lines $ pack input
      presents = map (map (read . unpack) . T.split (=='x')) lns
      ans1 = soln1 presents
      ans2 = soln2 presents
  print ans1
  print ans2

