import System.Environment (getArgs)
import System.Exit (die)
import Util (readInput, fmtInputPath)

import Y23.D01

main :: IO ()
main = do
    args <- getArgs
    case args of
        [year, day] -> case (year, day) of
            ("2023", "1") -> do
                input <- readInput year day
                Y23.D01.solve input
            _ -> die (year ++ " " ++ day ++ " not implemented")
        _ -> die "usage: aoc-hs <year> <day>"
