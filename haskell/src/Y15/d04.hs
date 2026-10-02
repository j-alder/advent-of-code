module Y25.D04 where


import Data.IntMap.Lazy (IntMap, fromList)
import qualified Data.IntMap.Lazy as IM
import Data.List (intersect)

getRollIndexes :: [Char] -> [Int] -> [Int]
getRollIndexes [] res = res
getRollIndexes ('@' : cs) res = getRollIndexes cs (length cs : res)
getRollIndexes ('.' : cs) res = getRollIndexes cs res

createMap :: [[Char]] -> [(Int, [Int])] -> IntMap [Int]
createMap [] lst = fromList lst
createMap (row : rest) lst = 
  let r = (length rest, getRollIndexes row []) : lst
  in createMap rest r

adjRolls :: (Int, Int) -> IntMap [Int] -> Int
adjRolls (a, b) map = 
  length $ x (IM.lookup (a + 1) map) (IM.lookup a map) (IM.lookup (a - 1) map)
  where
    x (Just u) (Just s) (Just d) =
      intersect [b + 1, b, b - 1] u ++ intersect [b + 1, b - 1] s ++ intersect [b + 1, b, b - 1] d
    x Nothing (Just s) (Just d) = 
      intersect [b + 1, b - 1] s ++ intersect [b + 1, b, b - 1] d
    x (Just u) (Just s) Nothing = 
      intersect [b + 1, b, b - 1] u ++ intersect [b + 1, b - 1] s
    x Nothing (Just s) Nothing = 
      [b + 1, b - 1] `intersect` s

foldRow :: [Char] -> Int -> Int -> IntMap [Int] -> Int
foldRow [] _ cnt _ = cnt
foldRow ('.' : cs) rowIdx cnt map = foldRow cs rowIdx cnt map
foldRow ('@' : cs) rowIdx cnt map =
  let nAdj = adjRolls (rowIdx, length cs) map
      cnt' = if nAdj < 4 then 1 else 0
  in foldRow cs rowIdx (cnt + cnt') map

-- foldr across the grid
-- foldr across each line
-- if char == '@', check neighbors
soln1 :: [[Char]] -> IntMap [Int] -> Int -> Int
soln1 [] _ cnt = cnt
soln1 (r : rs) m c = foldRow r (length rs) c m

main :: IO ()
main = do
  input <- readFile "../../input/y2025/d04.test.txt"
  let lns = lines input
      map = createMap lns []
      ans1 = soln1 lns map 0
  print ans1

