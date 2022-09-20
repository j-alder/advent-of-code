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

{- 
--- Day 2: I Was Told There Would Be No Math ---
The elves are running low on wrapping paper, and so they need to submit an order for more. They have a list of the dimensions (length l, width w, and height h) of each present, and only want to order exactly as much as they need.

Fortunately, every present is a box (a perfect right rectangular prism), which makes calculating the required wrapping paper for each gift a little easier: find the surface area of the box, which is 2*l*w + 2*w*h + 2*h*l. The elves also need a little extra paper for each present: the area of the smallest side.

For example:

A present with dimensions 2x3x4 requires 2*6 + 2*12 + 2*8 = 52 square feet of wrapping paper plus 6 square feet of slack, for a total of 58 square feet.
A present with dimensions 1x1x10 requires 2*1 + 2*10 + 2*10 = 42 square feet of wrapping paper plus 1 square foot of slack, for a total of 43 square feet.
All numbers in the elves' list are in feet. How many total square feet of wrapping paper should they order?

--- Part Two ---
The elves are also running low on ribbon. Ribbon is all the same width, so they only have to worry about the length they need to order, which they would again like to be exact.

The ribbon required to wrap a present is the shortest distance around its sides, or the smallest perimeter of any one face. Each present also requires a bow made out of ribbon as well; the feet of ribbon required for the perfect bow is equal to the cubic feet of volume of the present. Don't ask how they tie the bow, though; they'll never tell.

For example:

A present with dimensions 2x3x4 requires 2+2+3+3 = 10 feet of ribbon to wrap the present plus 2*3*4 = 24 feet of ribbon for the bow, for a total of 34 feet.
A present with dimensions 1x1x10 requires 1+1+1+1 = 4 feet of ribbon to wrap the present plus 1*1*10 = 10 feet of ribbon for the bow, for a total of 14 feet.
How many total feet of ribbon should they order?
-}
