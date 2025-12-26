module Y25.D01 where


data Dir = L | R

turnOnce :: Dir -> Int -> Int
turnOnce d p = mod x 100
  where 
    x = case d of
      R -> p + 1
      L -> p - 1

rotateCnt :: Dir -> Int -> Int -> Int -> (Int, Int)
rotateCnt d 0 pos cnt = (pos, cnt)
rotateCnt d rem pos cnt = 
  rotateCnt d (rem - 1) pos' (if pos' == 0 then cnt + 1 else cnt)
  where
    pos' = turnOnce d pos

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

soln2 :: [String] -> Int -> Int -> Int
soln2 [] pos zeroes = zeroes
soln2 (x : xs) pos zeroes =
  let
    d = head x
    dir = case d of
      'R' -> R
      _ -> L
    (a, b) = properFraction $ (read $ tail x :: Float) / 100
    (newPos, zs) = rotateCnt dir (round $ b * 100) pos a
  in soln2 xs newPos (zeroes + zs)

main :: IO ()
main = do
  input <- readFile "../../input/y2025/d01.txt"
  let lns = lines input
      ans1 = soln1 lns 50 0
      ans2 = soln2 lns 50 0
  print ans1
  print ans2

