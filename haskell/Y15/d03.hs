module Y15.D03 where

import Data.Set (Set, empty, insert)

soln2 :: [Char] -> Int
soln2 = go empty (0,0) (0,0)
  where
    go hSet _ _ [] = length hSet
    go hSet rPos sPos inst =
      let hSet' = insert (show rPos) hSet
          hSet'' = insert (show sPos) hSet'
          (sPos', d) = move sPos inst
          (rPos', d') = move rPos d
      in go hSet'' rPos' sPos' d'
    move pos [] = (pos, [])
    move (x,y) (n:ns) = case n of
      '<' -> ((x-1,y), ns)
      '>' -> ((x+1,y), ns)
      'v' -> ((x,y-1), ns)
      '^' -> ((x,y+1), ns)
      _ -> ((x,y), ns)

soln1 :: [Char] -> Int
soln1 input = getHouses input (0,0) empty
  where
    getHouses :: [Char] -> (Int, Int) -> Set [Char] -> Int
    getHouses [] _ s = length s
    getHouses (c:cs) (x,y) s =
      let p' = newPos c (x,y)
          s' = insert (show p') s
      in getHouses cs p' s'
    newPos c (x,y)
      | c == '<' = (x-1,y)
      | c == '>' = (x+1,y)
      | c == 'v' = (x,y-1)
      | c == '^' = (x,y+1)
      | otherwise = (x,y)

main :: IO ()
main = do
  input <- readFile "../../input/y2015/d03.txt"
  let ans1 = soln1 input
      ans2 = soln2 input
  print ans1
  print ans2
