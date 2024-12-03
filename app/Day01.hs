module Main where

import qualified Data.Text.IO as TIO
import qualified Libs.Parse as P
import Data.List (sort)
import Control.Applicative (Alternative(some))


main :: IO ()
main = do
  input <- either failShow pure . P.runOnce parser =<< TIO.readFile "input/day01"
  let xs = sort $ fmap fst input
  let ys = sort $ fmap snd input
  print $ sum . fmap abs $ zipWith subtract xs ys


  print $ sum $ do
    x <- xs
    pure $ x * length (filter (== x) ys)

parser :: P.Parser [(Int, Int)]
parser = some $ (,) <$> P.decimal <* P.spaces <*> P.decimal <* P.newline
-- parser = P.decimal <* P.spaces

failShow :: Show a => a -> IO b
failShow = fail . show

