module Y24.D02 where

import Util (splitByChar, fmtSolnWithRuntime)
import Data.Text (Text)
import qualified Data.Text as T

partOne :: Text -> Text
partOne = undefined

partTwo :: Text -> Text
partTwo = undefined

soln :: Text -> IO ()
soln input = do
  let input' = T.splitOn (T.singleton '\n') input
  fmtSolnWithRuntime (partOne input') (partTwo input')

{-

-}