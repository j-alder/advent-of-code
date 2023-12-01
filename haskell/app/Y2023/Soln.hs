module Y2023.Soln where
import Y2023.D01

ansForDay :: String -> String -> (String, String)
ansForDay day input
  | day == "1" = Y2023.D01.soln input
  | otherwise = ("incomplete", "incomplete")
