{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Main where

import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Set (Set)
import Data.Set qualified as S
import Data.Ix (Ix(inRange))
import Debug.Trace (traceShowId)

main :: IO ()
main = do
    (guard, grid, bounds) <- parser <$> TIO.readFile "input/day06"
    
    print $ S.size . traceShowId $ evalWalk grid ((0,0), bounds) guard

    -- print $ inRange ((0,0),(10,10)) (10,11)

data Cell = Space | Wall deriving (Show, Eq, Ord, Bounded, Enum)

type Point  = (Int, Int)
type Grid = Set (Int, Int)
data Dir = N | E | S | W deriving (Show, Eq, Ord, Enum, Bounded)


evalWalk :: Grid -> (Point,Point) -> Point -> Set Point
evalWalk grid bounds loc = go mempty (cycle [N,E,S,W]) loc where
  go !seen ds@(d:dss) loc
    | advance d loc `S.member` grid = go seen dss loc
    | inRange bounds (traceShowId loc) = go (S.insert loc seen) ds (advance d loc)
  go seen _ _ = seen
  advance :: Dir -> Point -> Point
  advance N (x,y) = (x,y-1)
  advance E (x,y) = (x+1,y)
  advance S (x,y) = (x,y+1)
  advance W (x,y) = (x-1,y)

parser :: Text -> (Point, Grid, Point)
parser t = let
    (guard, grid) = go (0,0) mempty 0 0 . T.unpack $ t
    ls = T.lines t
  in (guard, grid, (T.length $ head ls, length ls)) where
      go :: Point -> Grid -> Int -> Int -> String -> (Point, Grid)
      go !pos !grid !row !col ('.':cs) = go pos grid row (col + 1) cs
      go !pos !grid !row !col ('#':cs) = go pos (S.insert (col, row) grid) row (col + 1) cs
      go _    !grid !row !col ('^':cs) = go (col, row) grid row (col + 1) cs
      go !pos !grid !row _    ('\n':cs) = go pos grid (row + 1) 0 cs
      go !pos !grid _ _ _ = (pos, grid)
