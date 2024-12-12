{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Main where

import Libs.Parse (Parser)
import Libs.Parse qualified as P
import Data.Text.IO qualified as TIO
import Libs.V2
import qualified Data.Map as M
import Data.Char (isLetter)
import qualified Data.Set as S

main :: IO ()
main = do
  input <- P.runOnceIO parser =<< TIO.readFile "input/day12"
  let regions = foldMap singletonRegion <$> dfff input
  print $ sum . fmap fencePrice $ regions
  print $ sum . fmap bulkFencePrice $ regions


type Point = V2 Int
type Edge = (Point, Point)
type Grid a = M.Map Point a

parser :: Parser (Grid Char)
parser = P.grid P.newline (P.satisfies isLetter) M.singleton


data Region = Region {area :: S.Set Point, edges :: S.Set Edge, verts :: S.Set Point} deriving Show
instance Semigroup Region where
  (<>) :: Region -> Region -> Region
  x <> y = Region (area x <> area y) ((edges x S.\\ edges y) <> (edges y S.\\ edges x)) ((verts x S.\\ verts y) <> (verts y S.\\ verts x))
instance Monoid Region where
  mempty :: Region
  mempty = Region S.empty S.empty S.empty

singletonRegion :: Point -> Region
singletonRegion p = Region (S.singleton p) (S.fromList [(nw, ne), (nw, sw), (sw, se), (ne, se)]) (S.fromList [nw, ne, sw, se]) where
  nw = p
  ne = p + V2 1 0
  sw = p + V2 0 1
  se = p + V2 1 1

fencePrice :: Region -> Int
fencePrice (Region area edges _) = S.size area * S.size edges

bulkFencePrice :: Region -> Int
bulkFencePrice (Region area _ verts) = S.size area * (S.size verts + S.size missingVerts * 2 ) where
  -- plots are identified by their topleft corner, so offsetting gives us all the corners
  pointSets :: [S.Set Point]
  pointSets = [S.map (+d) area | d <- [0,V2 0 1, V2 1 0, V2 1 1]]

  -- verts in the center of checkerboards will be missing from the region
  missingVerts :: S.Set Point
  missingVerts = foldMap (S.filter (isCheckboard area)) pointSets

  -- checkboard returns all the points which sit inside a checkerboard pattern of squares
  isCheckboard area p = (S.size nhood == 2) && isDiag (S.elemAt 0 nhood) (S.elemAt 1 nhood) where
    nhood = area `S.intersection` S.fromList [p, p - V2 0 1, p - V2 1 0, p - V2 1 1]
    V2 a b `isDiag` V2 c d = a /= c && b /= d


dfff :: Grid Char -> [S.Set Point]
dfff grid = go (M.keysSet grid) where
  go :: S.Set Point -> [S.Set Point]
  go targets
    | S.null targets = []
    | otherwise = case S.findMin targets of
      p -> let component = flood (grid M.! p) S.empty S.empty [p] in component : go (targets S.\\ component)

  flood :: Char -> S.Set Point -> S.Set Point -> [Point] -> S.Set Point
  flood _ _ !acc [] = acc
  flood c !seen !acc (p:ps)
    | p `S.member` seen = flood c seen acc ps
    | grid M.!? p == Just c = flood c (S.insert p seen) (S.insert p acc) (neighbours p ++ ps)
    | otherwise = flood c (S.insert p seen) acc ps

  neighbours :: Point -> [Point]
  neighbours p = [p + V2 0 1, p + V2 1 0, p - V2 0 1, p - V2 1 0]