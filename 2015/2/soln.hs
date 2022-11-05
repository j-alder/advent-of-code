import Data.List (sort)
first :: ([Char], [Char]) -> [Char]
first (a, b) = a

second :: ([Char], [Char]) -> [Char]
second (a, b) = b

l :: Num a => (a, a, a) -> a
l (x, _, _) = x

w :: Num a => (a, a, a) -> a
w (_, x, _) = x

h :: Num a => (a, a, a) -> a
h (_, _, x) = x

boxTotal :: (Int, Int, Int) -> Int
boxTotal d = (2 * sa) + (2 * sb) + (2 * sc) + minimum [sa, sb, sc]
    where
        sa = l d * w d
        sb = w d * h d
        sc = h d * l d

getTriple :: String -> (Int, Int, Int)
getTriple str = (read (i !! 0), read (i !! 1), read (i !! 2))
    where
        i = splitStr 'x' str

splitStr :: Char -> String -> [String]
splitStr c str = if null n'' then [n']
    else n' : splitStr c (tail n'')
    where 
        n = span (/= c) str
        n' = first n
        n'' = second n

soln1 :: String -> Int
soln1 str = sum [ boxTotal t | t <- [ getTriple x | x <- splitStr '\n' str ] ]

ribbonTotal :: (Int, Int, Int) -> Int
ribbonTotal d = 2 * (mins !! 0) + 2 * (mins !! 1) + xtra
    where
        mins = tail (reverse (sort [l d, w d, h d]))
        xtra = l d * w d * h d

soln2 :: String -> Int
soln2 str = sum [ ribbonTotal t | t <- [ getTriple x | x <- splitStr '\n' str ]]

main = do
    input <- readFile "./input.txt"
    print (soln1 input)
    print (soln2 input)
