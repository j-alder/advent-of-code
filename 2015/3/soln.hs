import Data.Map (Map, insertWith, empty, keys)

type Position = (Int, Int)

move :: Position -> Char -> Position
move currPos direction = case direction of 
    '^' -> (fst currPos, snd currPos + 1)
    'v' -> (fst currPos, snd currPos - 1)
    '<' -> (fst currPos - 1, snd currPos)
    '>' -> (fst currPos + 1, snd currPos)
    _ -> currPos

allPos :: Position -> String -> [Position]
allPos currPos moves = if null moves 
    then []
    else currPos : allPos (move currPos (head moves)) (tail moves)

groupByPos :: Position -> Map Position Int -> Map Position Int
groupByPos pos = insertWith (+) pos 1

soln1 :: String -> Int
soln1 input = length (keys cMap)
    where
        cMap = foldr groupByPos empty (allPos (0, 0) input)

prep :: String -> [(Char, Char)] -> [(Char, Char)]
prep str acc 
    | null str = acc
    | length str == 1 = acc ++ [(head str, ' ')]
    | otherwise = prep (drop 2 str) (acc ++ [(head str, str !! 1)])

soln2 :: String -> Int
soln2 input = length (keys srMap)
    where
        moves = unzip (prep input [])
        sMap = foldr groupByPos empty (allPos (0, 0) (fst moves))
        srMap = foldr groupByPos sMap (allPos (0, 0) (snd moves))

main = do
    input <- readFile "./input.txt"
    print (soln1 input)
    print (soln2 input)
