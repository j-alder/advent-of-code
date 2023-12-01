module Y2022.Soln (showSoln) where
import Util (readInput, splitStr, allToInt)
import Y2022.D01
import Y2022.D02

showSoln :: String -> String -> IO ()
showSoln day input
  | day == "1" = print(Y2022.D01.soln1(allToInt (splitStr '\n' input)) 0 0)
  | day == "2" = print(Y2022.D02.soln1(splitStr '\n' input) 0)
  | otherwise = print("No solution for this day yet")
