module Main where
import Util (readInput, splitStr, allToInt, fmtSolnStr)
import Y2023.Soln
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  input <- readInput (head args) (args !! 1)
  case head args of
    "2023" -> putStrLn (fmtSolnStr (Y2023.Soln.ansForDay (args !! 1) input))
    _ -> print "No solution for this year yet"
