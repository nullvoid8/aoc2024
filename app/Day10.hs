{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Main where

import Libs.Parse (Parser)
import Libs.Parse qualified as P
import Data.Text.IO qualified as TIO
import Data.Map qualified as M
import Libs.V2 (V2(..))
import Data.Char (isDigit)
import Data.Set qualified as S
import Data.Monoid (Sum (..))
import Data.Foldable

main :: IO ()
main = do
  input <- P.runOnceIO parser =<< TIO.readFile "input/day10"
  print $ trailScore input
  print $ trailRating input


type Point = V2 Int
type Grid a = M.Map Point a

parser :: Parser (Grid Int)
parser = P.grid P.newline (read . (:[]) <$> P.satisfies isDigit) M.singleton

trailScore :: Grid Int -> Int
trailScore g = sum . fmap S.size . M.elems $ M.restrictKeys trailMap groundKeys where
  groundKeys :: S.Set Point
  groundKeys = M.keysSet $ M.filter (== 0) g

  neighbours :: Point -> [Point]
  neighbours p = [p + V2 0 1, p + V2 1 0, p - V2 1 0, p - V2 0 1]

  trail :: Point -> Int -> S.Set Point
  trail p 9 = S.singleton p
  trail p h = foldMap (trailMap M.!) $ filter (\q -> M.findWithDefault 0 q g - h == 1) (neighbours p)

  trailMap :: Grid (S.Set Point)
  trailMap = M.mapWithKey trail g

trailRating :: Grid Int -> Int
trailRating g = getSum . fold $ M.restrictKeys trailMap groundKeys where
  groundKeys :: S.Set Point
  groundKeys = M.keysSet $ M.filter (== 0) g

  neighbours :: Point -> [Point]
  neighbours p = [p + V2 0 1, p + V2 1 0, p - V2 1 0, p - V2 0 1]

  trail :: Point -> Int -> Sum Int
  trail _ 9 = 1
  trail p h = foldMap (trailMap M.!) $ filter (\q -> M.findWithDefault 0 q g - h == 1) (neighbours p)

  trailMap :: Grid (Sum Int)
  trailMap = M.mapWithKey trail g
