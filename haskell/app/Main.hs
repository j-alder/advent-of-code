module Main where
import Util (readInput, splitStr, allToInt)
import Y2022.Soln
import System.Environment

main = do
  args <- getArgs
  input <- readInput (args !! 0) (args !! 1)
  case args !! 0 of
    "2022" -> Y2022.Soln.showSoln (args !! 1) input
    _ -> print("No solution for this year yet")
