{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# LANGUAGE GADTs #-}

module Main (main) where

-- import Libs.Parse (Parser)
import Libs.Parse qualified as P
import Data.Text.IO qualified as TIO
import Libs.V2
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Monoid (First(..))
import Data.Foldable

main :: IO ()
main = do
  grid <- P.runOnceIO parser =<< TIO.readFile "input/day20"
  print $ length $ filter (100 <=) . shortcutUpto grid 2 =<< M.keys grid
  print $ length $ filter (100 <=) . shortcutUpto grid 20 =<< M.keys grid

type Point = V2 Int
type Grid a = M.Map Point a

parser :: P.Parser (Grid Int)
parser = scorer <$> P.grid P.newline (P.oneOf "#.SE") folder where
  folder p c = case c of
    '.' -> (mempty, S.singleton p)
    'S' -> (First $ Just p, S.singleton p)
    'E' -> (mempty, S.singleton p)
    _ -> mempty

  scorer :: (First Point, S.Set Point) -> Grid Int
  scorer (First (Just start), path) = follow M.empty (S.delete start path) start 0
  scorer _ = error "unreachable"

  follow :: Grid Int -> S.Set Point -> Point -> Int -> Grid Int
  follow acc path here cost
    | S.null path = M.insert here cost acc
    | otherwise   = let (Just next) = find ((== 1) . taxi . subtract here) path in follow (M.insert here cost acc) (S.delete next path) next (cost + 1)

shortcutUpto :: Grid Int -> Int -> Point -> [Int]
shortcutUpto g maxD p = fmap (\x -> g M.! x - currentCost - taxi (p - x)) reachable where
  currentCost = g M.! p
  -- x is reachable from p if it is within maxD and this shortcut is an improvement
  reachable = filter (\x -> let d = taxi $ p - x in d <= maxD && currentCost + d < g M.! x) $ M.keys g
  
