{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Libs.Parse (Parser)
import Libs.Parse qualified as P
import Data.Text.IO qualified as TIO
import Libs.V2 ( V2(..), reduce )
import qualified Data.Map as M
import Control.Applicative
import Data.Char (isAlphaNum)
import qualified Data.Set as S
import Data.Ix (inRange)
import Data.List (groupBy, sortBy)
import Data.Ord (comparing)
import Data.Maybe (catMaybes)

type Point = V2 Int

main :: IO ()
main = do
  (min, max, grid) <- P.runOnceIO parser =<< TIO.readFile "input/day08"
  let bounds = (min, max)

  print $ S.size $ foldMap
        (allAntinodesDbl bounds . fmap fst)
        (groupBy (\x y -> snd x == snd y) . sortBy (comparing snd) . M.toList $ grid)
  print $ S.size $ foldMap
        (allAntinodes bounds . fmap fst)
        (groupBy (\x y -> snd x == snd y) . sortBy (comparing snd) . M.toList $ grid)
  
parser :: Parser (Point, Point, M.Map Point Char)
parser = P.grid P.newline ((Nothing <$ P.char '.') <|> (Just <$> P.satisfies isAlphaNum))

allAntinodesDbl :: (Point, Point) -> [Point] -> S.Set Point
allAntinodesDbl bounds = go S.empty where
  go !acc [] = acc
  go !acc (x:xs) = go (S.unions $ acc : (S.fromList . antinodesDbl bounds x <$> xs)) xs


antinodesDbl :: (Point, Point) -> Point -> Point -> [Point]
antinodesDbl bounds x y = catMaybes [ toMaybe divisible $ x + smallDiff, toMaybe divisible $ y - smallDiff, toMaybe (inRange bounds leftNode) leftNode, toMaybe (inRange bounds rightNode) rightNode] where
  diff = y - x -- x -> y
  divisible = (rem <$> diff <*> 3) == 0
  smallDiff = div <$> diff <*> 3
  leftNode = x - diff
  rightNode = y + diff

toMaybe :: Bool -> a -> Maybe a
toMaybe True x = Just x
toMaybe False _ = Nothing

allAntinodes :: (Point, Point) -> [Point] -> S.Set Point
allAntinodes bounds = go S.empty where
  go !acc [] = acc
  go !acc (x:xs) = go (S.unions $ acc : (S.fromList . antinodes bounds x <$> xs)) xs


antinodes :: (Point, Point) -> Point -> Point -> [Point]
antinodes bounds x y = posNodes ++ negNodes where
  diff = reduce $ y - x -- x -> y
  posNodes = takeWhile (inRange bounds) $ iterate (+ diff) x
  negNodes = takeWhile (inRange bounds) $ iterate (subtract diff) y
  
