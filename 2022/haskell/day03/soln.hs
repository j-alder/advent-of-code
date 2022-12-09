import Data.List (elemIndex)

splitStr :: Char -> String -> [String]
splitStr c str = if null n'' then [n']
    else n' : splitStr c (tail n'')
    where
        n = span (/= c) str
        n' = fst n
        n'' = snd n

chars :: [Char]
chars = ['a'..'z'] ++ ['A'..'Z']

charVal :: Char -> Int
charVal c = case v of
    Just v -> v + 1
    Nothing -> 0
    where v = elemIndex c chars

bifurcate :: [Char] -> ([Char], [Char])
bifurcate str = splitAt (length str `div` 2) str

getPriority :: [Char] -> [Char] -> Int
getPriority c1 c2
    | null c1 = 0
    | otherwise = case d of
        Just d -> charVal c
        Nothing -> getPriority (tail c1) c2
        where 
            c = head c1
            d = elemIndex c c2

sumPriorities :: [[Char]] -> Int -> Int
sumPriorities input total
    | null input = total
    | otherwise = do
        sumPriorities (tail input) (total + uncurry getPriority split)
        where split = bifurcate (head input)

main = do
    input <- readFile "../../input/three.txt"
    print (sumPriorities (splitStr '\n' input) 0)