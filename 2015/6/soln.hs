type Coordinate = (Int, Int)
type Instruction = (String, (Int, Int), (Int, Int))

splitStr :: Char -> String -> [String]
splitStr c str = if null n'' then [n']
    else n' : splitStr c (tail n'')
    where
        n = span (/= c) str
        n' = fst n
        n'' = snd n

fmtCoord :: String -> (Int, Int)
fmtCoord s = (read (spl !! 0), read (spl !! 1))
    where
        spl = splitStr ',' s

fmtInst :: String -> [Instruction] -> [Instruction]
fmtInst s acc = 
    if (spl !! 1 == "on") 
        then ("on", fmtCoord (spl !! 2), fmtCoord (spl !! 4)) : acc
    else if (spl !! 1 == "off")
        then ("off", fmtCoord (spl !! 2), fmtCoord (spl !! 4)) : acc
    else if (spl !! 0 == "toggle")
        then ("toggle", fmtCoord (spl !! 1), fmtCoord (spl !! 3)) : acc
    else acc
    where
        spl = splitStr ' ' s

parseInput :: [String] -> [Instruction]
parseInput input = foldr fmtInst [] input

initialGrid :: [[Int]]
initialGrid = replicate 1000 (replicate 1000 0)

-- (45, 56) (100, 90)
-- start: ((i !! 45) !! 56)
-- 45 -> 100
-- 59 -> 90
-- 45, 59

type Mutator = Coordinate -> Coordinate -> [[Int]] -> [[Int]] 

onGrid :: Mutator
onGrid s e g = map () g

offGrid :: Mutator
offGrid s e g = foldr _ [] g

toggleGrid :: Mutator
toggleGrid s e g = foldr _ [] g

doInst :: Instruction -> [[Int]] -> [[Int]]
doInst i g = case (i !! 0) of 
    ("on") -> onGrid (i !! 1) (i !! 2) g
    ("off") -> offGrid (i !! 1) (i !! 2) g
    ("toggle") -> toggleGrid (i !! 1) (i !! 2) g
    _ -> g

soln :: IO ()
soln = do
    raw <- readFile "input.txt"
    let input = splitStr '\n' raw
    let inst = parseInput input
    print inst

{-
--- Day 6: Probably a Fire Hazard ---
Because your neighbors keep defeating you in the holiday house decorating contest 
year after year, you've decided to deploy one million lights in a 1000x1000 grid.

Furthermore, because you've been especially nice this year, Santa has mailed you 
instructions on how to display the ideal lighting configuration.

Lights in your grid are numbered from 0 to 999 in each direction; the lights at 
each corner are at 0,0, 0,999, 999,999, and 999,0. The instructions include 
whether to turn on, turn off, or toggle various inclusive ranges given as 
coordinate pairs. Each coordinate pair represents opposite corners of a rectangle, 
inclusive; a coordinate pair like 0,0 through 2,2 therefore refers to 9 lights in 
    a 3x3 square. The lights all start turned off.

To defeat your neighbors this year, all you have to do is set up your lights by 
doing the instructions Santa sent you in order.

For example:

turn on 0,0 through 999,999 would turn on (or leave on) every light.

toggle 0,0 through 999,0 would toggle the first line of 1000 lights, turning off 
the ones that were on, and turning on the ones that were off.

turn off 499,499 through 500,500 would turn off (or leave off) the middle four 
lights.

After following the instructions, how many lights are lit?
-}
