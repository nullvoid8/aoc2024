{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Main where

import Libs.Parse (Parser)
import Libs.Parse qualified as P
import Data.Text.IO qualified as TIO
import Libs.V2
import Data.Monoid (Sum(..))
import Data.Foldable (minimumBy)
import Data.Ord (comparing)

main :: IO ()
main = do
  input <- P.runOnceIO parser =<< TIO.readFile "input/day14"
  let bounds = V2 101 103
  print $ safetyFactor bounds $ fmap (simulateFor bounds 100) input
  print $ minimalSafety bounds input
  
type Robot = (V2 Int, V2 Int)

parser :: Parser [Robot]
parser = P.lines $ (,) <$> attribute "p" vec <* P.spaces <*> attribute "v" vec where
  vec = V2 <$> P.signed P.decimal <* P.char ',' <*> P.signed P.decimal
  attribute name value = P.text name *> P.char '=' *> value

simulateFor :: V2 Int -> Int -> Robot -> Robot
simulateFor bounds steps (p, v) = (p', v) where
  p' = mod <$> (p + (pure steps * v)) <*> bounds

safetyFactor :: V2 Int -> [Robot] -> Int
safetyFactor bounds = (\(Sum w, Sum x, Sum y, Sum z) -> w * x * y * z) . foldMap quadrant where
  mid = div <$> bounds <*> 2
  quadrant (p, _) = case compare <$> p <*> mid of
    V2 EQ _ -> mempty
    V2 _ EQ -> mempty
    V2 LT LT -> (1, 0, 0, 0)
    V2 LT GT -> (0, 1, 0, 0)
    V2 GT LT -> (0, 0, 1, 0)
    V2 GT GT -> (0, 0, 0, 1)

minimalSafety :: V2 Int -> [Robot] -> Int
minimalSafety bounds@(V2 bx by) robots = i where
  allGrids = [(safetyFactor bounds g, i)| i <- [0..(bx*by)], let g = fmap (simulateFor bounds i) robots]
  (_, i) = minimumBy (comparing fst) allGrids