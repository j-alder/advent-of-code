splitStr :: Char -> String -> [String]
splitStr c str = if null n'' then [n']
    else n' : splitStr c (tail n'')
    where
        n = span (/= c) str
        n' = fst n
        n'' = snd n

soln1 :: [Int] -> Int

main = do
  input <- readFile "../../input/one.txt"
  print (splitStr '\n' input)