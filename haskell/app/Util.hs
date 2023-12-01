module Util where

readInput :: String -> String -> IO String
readInput year day = 
  readFile ("../input/y" ++ year ++ "/d" ++ day' ++ ".txt")
  where
    day' = if length day == 1 then "0" ++ day else day

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

fmtSolnStr :: (String, String) -> String
fmtSolnStr ans = "Part 1: " ++ fst ans ++ "\nPart 2: " ++ snd ans ++ "\n"
