module Main where

import qualified Data.Text.IO as TIO
import qualified Libs.Parse as P
import Control.Applicative (Alternative(some))


main :: IO ()
main = do
  input <- P.runParser parser <$> TIO.readFile "input/day1"
  print input

parser :: P.Parser [(Int, Int)]
parser = some $ (,) <$> P.decimal <* P.spaces <*> P.decimal <* P.newline



