import Data.Map (Map, insertWith, empty, keys, toList)

contains :: String -> String -> Bool
contains s1 s2 
    | length s2 < length s1 = False
    | take (length s1) s2 == s1 = True
    | otherwise = contains s1 (tail s2)

splitStr :: Char -> String -> [String]
splitStr c str = if null n'' then [n']
    else n' : splitStr c (tail n'')
    where 
        n = span (/= c) str
        n' = fst n
        n'' = snd n

containsNVowels :: String -> Int -> Int -> Bool
containsNVowels s n c 
    | c >= n = True
    | null s = False
    | head s `elem` "aeiou" = containsNVowels (tail s) n (c + 1)
    | otherwise = containsNVowels (tail s) n c

containsTwiceLetter :: String -> Char -> Bool
containsTwiceLetter s c
    | null s = False
    | head s == c = True
    | otherwise = containsTwiceLetter (tail s) (head s)

forbidden :: [String]
forbidden = ["ab", "cd", "pq", "xy"]

containsForbidden :: String -> Bool
containsForbidden s
    | length s < 2 = False
    | take 2 s `elem` forbidden = True
    | otherwise = containsForbidden (tail s)

countNice :: String -> Int -> Int
countNice s n =
    if not (containsForbidden s) 
        && containsTwiceLetter s '_' && containsNVowels s 3 0
        then n + 1
    else n

soln1 :: IO ()
soln1 = do
    raw <- readFile "input.txt"
    print (foldr countNice 0 (splitStr '\n' raw))

containsPalindrome :: String -> Bool
containsPalindrome s 
    | length s < 3 = False
    | take 3 s == reverse (take 3 s) = True
    | otherwise = containsPalindrome (tail s)

buildMap :: String -> Map String Int -> Map String Int
buildMap s m
    | length s < 2 = m 
    | otherwise = buildMap (tail s) (insertWith (+) (take 2 s) 1 m)

isRepeat :: (String, Int) -> String -> Bool
isRepeat t s = head str /= last str || snd t == 3
    where
        str = fst t

containsRepeat :: String -> Bool
containsRepeat s = any (`isRepeat` s) (filter (\x -> snd x > 1) (toList (buildMap s empty)))

countNice2 :: String -> Int -> Int
countNice2 s n = 
    if containsPalindrome s && containsRepeat s then n + 1
    else n

soln2 :: IO ()
soln2 = do
    raw <- readFile "input.txt"
    print (foldr countNice2 0 (splitStr '\n' raw))

filterNice :: IO ()
filterNice = do
    raw <- readFile "input.txt"
    let input = splitStr '\n' raw
    print (filter (\x -> not (containsPalindrome x || containsRepeat x)) input)
    print (filter (\x -> containsPalindrome x && containsRepeat x) input)

{- 
--- Day 5: Doesn't He Have Intern-Elves For This? ---
Santa needs help figuring out which strings in his text file are naughty or nice.

A nice string is one with all of the following properties:

It contains at least three vowels (aeiou only), like aei, xazegov, or aeiouaeiouaeiou.
It contains at least one letter that appears twice in a row, like xx, abcdde (dd), 
or aabbccdd (aa, bb, cc, or dd).
It does not contain the strings ab, cd, pq, or xy, even if they are part of one of 
the other requirements.
For example:

ugknbfddgicrmopn is nice because it has at least three vowels (u...i...o...), a 
double letter (...dd...), and none of the disallowed substrings.

aaa is nice because it has at least three vowels and a double letter, even though 
the letters used by different rules overlap.

jchzalrnumimnmhp is naughty because it has no double letter.

haegwjzuvuyypxyu is naughty because it contains the string xy.

dvszwmarrgswjxmb is naughty because it contains only one vowel.

How many strings are nice?

--- Part Two ---
Realizing the error of his ways, Santa has switched to a better model of determining 
whether a string is naughty or nice. None of the old rules apply, as they are all 
clearly ridiculous.

Now, a nice string is one with all of the following properties:

It contains a pair of any two letters that appears at least twice in the string 
without overlapping, like xyxy (xy) or aabcdefgaa (aa), but not like aaa (aa, but it 
overlaps).
It contains at least one letter which repeats with exactly one letter between them, 
like xyx, abcdefeghi (efe), or even aaa.

For example:

qjhvhtzxzqqjkmpb is nice because is has a pair that appears twice (qj) and a letter 
that repeats with exactly one letter between them (zxz).
xxyxx is nice because it has a pair that appears twice and a letter that repeats 
with one between, even though the letters used by each rule overlap.
uurcxstgmygtbstg is naughty because it has a pair (tg) but no repeat with a single 
letter between them.
ieodomkazucvgmuy is naughty because it has a repeating letter with one between 
(odo), but no pair that appears twice.

How many strings are nice under these new rules?
-}