module Y24.D01 where

import Util (splitByChar)
import Data.Text (Text, pack)

soln :: Text -> (Text, Text)
soln input = do
  let input' = splitByChar '\n' input
  (pack "incomplete", pack "incomplete")
