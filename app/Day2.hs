{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Main where

import qualified Data.Text.IO as TIO
import qualified Libs.Parse as P
import Data.List (inits, tails)


main :: IO ()
main = do
  input <- either failShow pure . P.runOnce parser =<< TIO.readFile "input/day2"
  print input
  print $ part1 input
  print $ part2 input

part1 :: [[Int]] -> Int
part1 = length . filter isSafe

part2 :: [[Int]] -> Int
part2 = length . filter (any isSafe . expand)
  where
    expand :: [Int] -> [[Int]]
    expand xs = xs : zipWith (++) (inits xs) (tail $ tails xs)

isSafe :: [Int] -> Bool
isSafe xs = let diffs = zipWith subtract xs (tail xs) in any (all (\x -> 1 <= x && x <= 3)) [diffs, fmap negate diffs]

parser :: P.Parser [[Int]]
parser = (P.decimal `P.sepBy` P.spaces) `P.sepBy` P.newline

failShow :: Show a => a -> IO b
failShow = fail . show

