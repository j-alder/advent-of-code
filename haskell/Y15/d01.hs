{-# LANGUAGE LambdaCase #-}

module Y15.D01 where

import qualified Data.Text as T
import Data.Text (Text, pack)

elev :: Int -> Char -> Int
elev n = \case
  '(' -> n + 1
  ')' -> n - 1
  _ -> n

soln2 :: Text -> Int
soln2 input = findBsmtPos input 0 1
  where
    findBsmtPos inst acc pos =
      if T.length inst == 0 then -1
      else
        let acc' = elev acc $ T.head inst
        in case acc' of
          -1 -> pos
          _ -> findBsmtPos (T.tail inst) acc' (pos + 1)

soln1 :: Text -> Int
soln1 = T.foldl elev 0

main :: IO ()
main = do
  input <-  readFile "../../input/y2015/d01.txt"
  let input' = pack input
      ans1 = soln1 $ pack input
      ans2 = soln2 $ pack input
  print ans1
  print ans2

