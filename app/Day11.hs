{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Main where

import Libs.Parse (Parser)
import Libs.Parse qualified as P
import Data.Text.IO qualified as TIO
import Data.Map qualified as M

main :: IO ()
main = do
  input <- P.runOnceIO parser =<< TIO.readFile "input/day11"
  print $ sum $ eval 25 input
  print $ sum $ eval 75 input

type Stones = M.Map Integer Integer

parser :: Parser Stones
parser = M.fromList . fmap (,1) <$> P.decimal `P.sepBy` P.spaces

log10 :: Integer -> Integer
log10 x
  | x < 10 = 1
  | x < 100 = 2
  | x < 1000 = 3
  | x < 10000 = 4
  | x < 100000 = 5
  | x < 1000000 = 6
  | x < 10000000 = 7
  | x < 100000000 = 8
  | x < 1000000000 = 9
  | x < 10000000000 = 10
  | x < 100000000000 = 11
  | x < 1000000000000 = 12
  | otherwise = 1 + log10 (x `div` 10)

step :: Stones -> Stones
step = M.unionsWith (+) . fmap go . M.toList where
  go :: (Integer, Integer) -> Stones
  go (0, n) = M.singleton 1 n
  go (x, n)
      | even log && l == r = M.fromList [(l, 2*n)]
      | even log && l /= r = M.fromList [(l, n), (r, n)]
      | otherwise = M.singleton (2024 * x) n
    where
      log = log10 x
      (l, r) = x `divMod ` (10 ^ (log `div` 2))

eval :: Int -> Stones -> Stones
eval n = (!! n) . iterate step