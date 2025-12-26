module Y25.D02 where


split :: Char -> String -> [String]
split c s = case rest of
  [] -> [chunk]
  _:rest -> chunk : split c rest
  where (chunk, rest) = break (==c) s

isDoubled :: [Char] -> Bool
isDoubled str = s == e
  where
    s = take (div (length str) 2) str
    e = drop (div (length str) 2) str

isInvalid :: [Char] -> Bool
isInvalid str = even (length str) && isDoubled str

sumInvalid :: [String] -> Int -> Int
sumInvalid [] cnt = cnt
sumInvalid (x : xs) cnt =
  if isInvalid x 
    then sumInvalid xs (cnt + read x)
  else sumInvalid xs cnt

soln1 :: [String] -> Int -> Int
soln1 [] cnt = cnt
soln1 (x : xs) cnt =
  let
    [start, end] = split '-' x
    strvals = map show [(read start :: Int)..(read end :: Int)]
    evenstrvals = filter (even . length) strvals
    inv = sumInvalid evenstrvals 0
  in soln1 xs (cnt + inv)

chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk n lst = take n lst : chunk n (drop n lst)

allEqual :: [String] -> Bool
allEqual (x : xs) = all (== x) xs

isInvalid' :: [Int] -> [Char] -> Bool
isInvalid' [] (x : xs) = all (== x) xs
isInvalid' (f : fs) str = allEqual (chunk f str) || isInvalid' fs str

-- got soln from reddit
isInvalid2 :: String -> Bool
isInvalid2 str = any testN [1 .. (len `div` 2)]
  where
    len = length str
    testN n = str == concat (replicate (len `div` n) (take n str))

factors :: Int -> [Int]
factors n = [x | x <- [2..n `div` 2], n `mod` x == 0]

sumInvalid' :: [String] -> Int -> Int
sumInvalid' [] cnt = cnt
sumInvalid' (x : xs) cnt = 
  -- if isInvalid' (factors $ length x) x
  if isInvalid2 x
    then sumInvalid' xs (cnt + read x)
  else sumInvalid' xs cnt

soln2 :: [String] -> Int -> Int
soln2 [] cnt = cnt
soln2 (x : xs) cnt =
  let
    [start, end] = split '-' x
    strvals = map show [(read start :: Int)..(read end :: Int)]
    inv = sumInvalid' strvals 0
  in soln2 xs (cnt + inv)

main :: IO ()
main = do
  input <- readFile "../../input/y2025/d02.txt"
  let
    strRanges = split ',' input
    ans1 = soln1 strRanges 0
    ans2 = soln2 strRanges 0
  print ans1
  print ans2

