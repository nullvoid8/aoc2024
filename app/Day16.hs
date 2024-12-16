{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Main (main) where

import Libs.Parse (Parser)
import Libs.Parse qualified as P
import Data.Text.IO qualified as TIO
import qualified Data.Set as S
import Libs.V2
import Data.Semigroup (First(..))
import Data.Maybe (fromJust)
import qualified Data.IntMap as M

main :: IO ()
main = do
  (start, end, grid) <- P.runOnceIO parser =<< TIO.readFile "input/day16"
  print $ astar start end grid

type Point = V2 Int
type Grid = S.Set Point

parser :: Parser (Point, Point, Grid)
parser = fmap (\(x, y, z) -> (getFirst $ fromJust x, getFirst $ fromJust y, z)) $ P.grid P.newline (P.oneOf "SE.#") $ \p c -> case c of
  'S' -> (Just $ First p, mempty, mempty)
  'E' -> (mempty, Just $ First p, mempty)
  '#' -> (mempty, mempty, S.singleton p)
  _ -> mempty

type PQ a = M.IntMap (S.Set a)

singletonPQ :: Ord a => Int -> a -> PQ a
singletonPQ p x = M.singleton p $ S.singleton x

push :: (Ord a) => Int -> a -> PQ a -> PQ a
push p x = M.insertWith S.union p (S.singleton x)

pushes :: Ord a => [(Int, a)] -> PQ a -> PQ a
pushes xs q = M.unionWith S.union q $ M.fromListWith S.union $ fmap (fmap S.singleton) xs

pop :: (Ord a) => PQ a -> Maybe (Int, a, PQ a)
pop q = case M.minViewWithKey q of
  Nothing -> Nothing
  Just ((p, xs), q) -> case S.minView xs of
    Nothing -> Nothing
    Just (x, xs) -> Just (p, x, if S.null xs then q else M.insert p xs q)

data Dir = N | E | S | W deriving (Eq, Ord, Show, Enum, Bounded)

left, right :: Dir -> Dir
left N = W
left x = pred x

right W = N
right x = succ x

advance :: Dir -> Point -> Point
advance N p = p - V2 0 1
advance E p = p + V2 1 0
advance S p = p + V2 0 1
advance W p = p - V2 1 0

type AStarLoc = (Point, Dir)
type AStarState = (Point, Dir, Int)

astar :: Point -> Point -> Grid -> Int
astar start end g = go S.empty (singletonPQ 0 (start, E, 0)) where
  go :: S.Set AStarLoc -> PQ AStarState -> Int
  go seen q = case pop q of
      Nothing -> 0
      Just (_, (p, d, c), q)
        | (p,d) `S.member` seen -> go seen q
        | p == end -> c
        | otherwise -> let
            estimate :: AStarState -> (Int, AStarState)
            estimate x@(p, _, c) = (c + taxi p end, x)
            neighbours :: [(Int, AStarState)]
            neighbours = estimate <$> filter (\(p,_,_) -> S.notMember p g) [(advance d p, d, c + 1), (p, left d, c + 1000), (p, right d, c + 1000)]
            in go (S.insert (p, d) seen) $ pushes neighbours q


  taxi :: Point -> Point -> Int
  taxi x y = sum . fmap abs $ subtract <$> x <*> y

