module Y25.D03 where


maxJoltage :: [Char] -> (Maybe Char, Maybe Char) -> Int
maxJoltage [] (Just t, Just o) = read [t, o]
maxJoltage (x : xs) (Nothing, Nothing) = maxJoltage xs (Just x, Nothing)
maxJoltage (x : xs) (Just t, Nothing) = maxJoltage xs (Just t, Just x)
maxJoltage (x : xs) (Just t, Just o) 
  | x > t && not (null xs) = maxJoltage xs (Just x, Nothing)
  | x > o = maxJoltage xs (Just t, Just x)
  | otherwise = maxJoltage xs (Just t, Just o)

joltageT :: [Char] -> (Maybe Char, Maybe Char)
joltageT = foldr (\c to -> 
  case to of
    (Nothing, Nothing) -> (Nothing, Just c)
    (Nothing, Just o) -> (Just o, Just c)
    (Just t, Just o) ->
      let
        t' = max c t
        o' = if t' == c then max o t else max o c
      in (Just t', Just o')
  ) (Nothing, Nothing)

joltage :: [Char] -> Int
joltage = 
  let j = joltageT
  in read [fst j, snd j]

soln1 :: [String] -> Int
soln1 = foldl (\t b -> t + maxJoltage b (Nothing, Nothing)) 0

main :: IO ()
main = do
  input <- readFile "../../input/y2025/d03.txt"
  let lns = lines input
      s1 = soln1 lns
  print s1

