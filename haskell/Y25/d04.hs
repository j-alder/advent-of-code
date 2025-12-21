module Y25.D04 where


neighbors :: [[Char]] -> Int -> Int -> [Char]
neighbors grid a b = 
  [ v (a - 1) (b - 1)
  , v (a - 1) b
  , v (a - 1) (b + 1)
  , v (a + 1) (b - 1)
  , v (a + 1) b
  , v (a + 1) (b + 1)
  , v a (b - 1)
  , v a (b + 1)
  ]
  where
    v x y = (grid !! x) !! y

adjRolls :: [[Char]] -> Int -> Int -> Int
adjRolls grid a b =
  foldl (\acc c -> if c == '@' then acc + 1 else acc) 0 (neighbors grid a b) 

soln1 :: [[Char]] -> Int
soln1 = undefined

main :: IO ()
main = do
  input <- readFile "../../../input/y2025/d03.txt"
  let lns = lines input
      s1 = soln1 lns
  print s1
