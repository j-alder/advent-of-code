module Y2022.D02 where
import Util

playPt :: String -> Int
playPt str = case str !! 2 of 
    'X' -> 1
    'Y' -> 2
    'Z' -> 3
    _ -> 0

matchPt :: String -> Int
matchPt str = case str of
    "A X" -> 3
    "A Y" -> 6
    "B Y" -> 3
    "B Z" -> 6
    "C X" -> 6
    "C Z" -> 3
    _ -> 0

outcome :: String -> Int
outcome str = playPt str + matchPt str

outcome2 :: String -> Int
outcome2 str = case str of
    "A X" -> outcome "A Z"
    "A Y" -> outcome "A X"
    "A Z" -> outcome "A Y"
    "B X" -> outcome str
    "B Y" -> outcome str
    "B Z" -> outcome str
    "C X" -> outcome "C Y"
    "C Y" -> outcome "C Z"
    "C Z" -> outcome "C X"

soln1 :: [String] -> Int -> Int
soln1 input total
    | null input = total
    | otherwise = soln1 (tail input) (outcome (head input) + total)

soln2 :: [String] -> Int -> Int
soln2 input total
    | null input = total
    | otherwise = soln2 (tail input) (outcome2 (head input) + total)
