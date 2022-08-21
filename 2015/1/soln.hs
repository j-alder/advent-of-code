
accum :: Char -> Int -> Int
accum c i = case c of 
    '(' -> i + 1
    ')' -> i - 1
    _ -> i

soln1 :: String -> Int
soln1 = foldr accum 0

popTillDrop :: String -> Int -> Int -> Int
popTillDrop str idx tot = case () of
    _ | null str -> 0
    _ | newTot < 0 -> idx
    _ -> popTillDrop (tail str) (idx + 1) newTot
    where
        newTot = accum (head str) tot

soln2 :: String -> Int
soln2 s = popTillDrop s 1 0

main = do
    input <- readFile "./input.txt"
    print (soln1 input)
    print (soln2 input)
