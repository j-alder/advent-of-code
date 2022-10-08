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
prep str acc = if (null str) then acc
    else if (length str == 1) then acc ++ [(str !! 0, ' ')]
    else prep (drop 2 str) (acc ++ [(str !! 0, str !! 1)])

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


{- 
--- Day 3: Perfectly Spherical Houses in a Vacuum ---
Santa is delivering presents to an infinite two-dimensional grid of houses.

He begins by delivering a present to the house at his starting location, 
and then an elf at the North Pole calls him via radio and tells him where 
to move next. Moves are always exactly one house to the north (^), 
south (v), east (>), or west (<). After each move, he delivers another 
present to the house at his new location.

However, the elf back at the north pole has had a little too much eggnog, 
and so his directions are a little off, and Santa ends up visiting some 
houses more than once. How many houses receive at least one present?

For example:

> delivers presents to 2 houses: one at the starting location, and 
one to the east.

^>v< delivers presents to 4 houses in a square, including twice to the 
house at his starting/ending location.

^v^v^v^v^v delivers a bunch of presents to some very lucky children at 
only 2 houses.

--- Part Two ---
The next year, to speed up the process, Santa creates a robot version of 
himself, Robo-Santa, to deliver presents with him.

Santa and Robo-Santa start at the same location (delivering two presents 
to the same starting house), then take turns moving based on instructions 
from the elf, who is eggnoggedly reading from the same script as the 
previous year.

This year, how many houses receive at least one present?

For example:

^v delivers presents to 3 houses, because Santa goes north, and then 
Robo-Santa goes south.

^>v< now delivers presents to 3 houses, and Santa and Robo-Santa end up 
back where they started.

^v^v^v^v^v now delivers presents to 11 houses, with Santa going one 
direction and Robo-Santa going the other.
-}    
