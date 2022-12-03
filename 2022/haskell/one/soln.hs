splitStr :: Char -> String -> [String]
splitStr c str = if null n'' then [n']
    else n' : splitStr c (tail n'')
    where
        n = span (/= c) str
        n' = fst n
        n'' = snd n

allToInt :: [String] -> [Int]
allToInt strArr 
  | null strArr = []
  | head strArr == "" = 0 : allToInt (tail strArr)
  | otherwise = (read (head strArr) :: Int) : allToInt (tail strArr)

soln1 :: [Int] -> Int -> Int -> Int
soln1 input currTotal highest 
  | null input = highest
  | head input == 0 = soln1 (tail input) 0 (max currTotal highest)
  | otherwise = soln1 (tail input) (currTotal + head input) highest

first :: (Int, Int, Int) -> Int
first (x, _, _) = x

second :: (Int, Int, Int) -> Int
second (_, y, _) = y

third :: (Int, Int, Int) -> Int
third (_, _, z) = z

newTrip :: Int -> (Int, Int, Int) -> (Int, Int, Int)
newTrip comp trip 
  | comp >= first trip = (comp, first trip, second trip)
  | comp >= second trip = (first trip, comp, second trip)
  | comp >= third trip = (first trip, second trip, comp)
  | otherwise = trip

soln2 :: [Int] -> Int -> (Int, Int, Int) -> Int
soln2 input currTotal topThree
  | null input = first topThree + second topThree + third topThree
  | head input == 0 = soln2 (tail input) 0 (newTrip currTotal topThree)
  | otherwise = soln2 (tail input) (currTotal + head input) topThree

main = do
  input <- readFile "../../input/one.txt"
  print(soln1 (allToInt (splitStr '\n' input)) 0 0)
  print(soln2 (allToInt (splitStr '\n' input)) 0 (0, 0, 0))
