module Y25.D01 where


rotate :: [Char] -> Int -> Int
rotate (x : xs) pos
  | x == 'R' = mod (pos + read xs) 100
  | otherwise = mod (pos - read xs) 100
 
soln1 :: [String] -> Int -> Int -> Int
soln1 [] pos zeroes = zeroes
soln1 (x : xs) pos zeroes =
  let
    newPos = rotate x pos
    z = if newPos == 0 then 1 else 0
  in soln1 xs newPos (zeroes + z)

main :: IO ()
main = do
  input <- readFile "../../input/y2025/d01.txt"
  let lns = lines input
      ans1 = soln1 lns 50 0
  print ans1

