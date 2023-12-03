module Y2023.Soln where
import Y2023.D01
import Y2023.D02

ansForDay :: String -> String -> (String, String)
ansForDay day input
  | day == "1" = Y2023.D01.soln input
  | day == "2" = Y2023.D02.soln input
  | otherwise = ("incomplete", "incomplete")
