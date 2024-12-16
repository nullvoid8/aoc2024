{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE OverloadedStrings, OverloadedLists #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Main (main) where

import Libs.Parse (Parser)
import Libs.Parse qualified as P
import Data.Text.IO qualified as TIO
import Data.Maybe (mapMaybe, fromJust)
import qualified Data.Map as M
import Libs.V2 (V2(..))
import Data.Monoid (First (..), Sum (..), Endo (..), Dual (..))
import Control.Applicative
import Data.Set qualified as S

main :: IO ()
main = do
  (p,g,cmd) <- P.runOnceIO parser =<< TIO.readFile "input/day15"
  print $ score . snd . (`appEndo` (p,g)) . getDual $ foldMap (Dual . Endo . step) cmd
  print $ scoreWide . snd . (`appEndo` widen (p, g)) . getDual $ foldMap (Dual . Endo . stepWide) cmd


data Cell = Wall | Box deriving (Show, Eq, Ord, Enum, Bounded)
type Dir = V2 Int

type Point = V2 Int
type Grid a = M.Map Point a

up, down, left, right :: Point
up = V2   0 (-1)
right = V2   1   0
down = V2   0   1
left = V2 (-1)  0

parser :: Parser (Point, Grid Cell, [Dir])
parser = mapOut <$> grid <* P.newline <* P.newline <*> commands where
  mapOut (First (Just x),y) z = (x,y,z)
  mapOut _ _ = error "unreachable"
  grid = P.grid P.newline (P.oneOf "#O@.") $ \p cell -> case cell of
    '#' -> (mempty, M.singleton p Wall)
    'O' -> (mempty, M.singleton p Box)
    '@' -> (pure p, mempty)
    _ -> mempty
  commands = mapMaybe cmds <$> some (P.oneOf "<^>v\n")
  cmds '^' = Just up
  cmds '>' = Just right
  cmds 'v' = Just down
  cmds '<' = Just left
  cmds _ = Nothing

step :: Dir -> (Point, Grid Cell) -> (Point, Grid Cell)
step d (p, g) = case g M.!? (p + d) of
    Nothing -> (p + d, g)
    Just Wall -> (p, g)
    Just Box -> case findSpace $ tail $ iterate (+d) p of
      Just q -> (p + d, M.insert q Box $ M.delete (p + d) g)
      Nothing -> (p, g)
  where
    findSpace :: [Point] -> Maybe Point
    findSpace = fromJust . getFirst . foldMap (\p -> case g M.!? p of
      Just Wall -> pure Nothing
      Just Box -> mempty
      Nothing -> pure $ Just p)

score :: Grid Cell -> Int
score = getSum . M.foldMapWithKey (\(V2 x y) c -> case c of
  Wall -> 0
  Box -> Sum $ 100 * y + x)

data WideCell = WWall | BoxL | BoxR deriving Show

widen :: (Point, Grid Cell) -> (Point, Grid WideCell)
widen (p,g) = (skew p, M.fromList . concatMap widen . M.toList $ g) where
  skew p = p * V2 2 1
  widen (p, Wall) = [(skew p , WWall), (skew p + right, WWall)]
  widen (p, Box) = [(skew p, BoxL), (skew p + right, BoxR)]

stepWide :: Dir -> (Point, Grid WideCell) -> (Point, Grid WideCell)
stepWide d (p, g) = case g M.!? (p + d) of
    Nothing -> (p+d, g)
    Just WWall -> (p, g)
    Just BoxL -> case tryMove $ p+d of
      Nothing -> (p, g)
      Just bs -> (p+d, shiftBoxes d bs g)
    Just BoxR -> case tryMove $ p + d + left of
      Nothing -> (p, g)
      Just bs -> (p+d, shiftBoxes d bs g)
  where
    tryMove p = case d of
      d
        | d == left -> tryMoveL p
        | d == right -> tryMoveR p
        | otherwise -> tryMoveUD p

    tryMoveL, tryMoveR, tryMoveUD :: Point -> Maybe [Point]
    tryMoveL p = (p:) <$> case g M.!? (p + left) of
        Nothing -> Just []
        Just BoxR -> tryMoveL (p + 2 * left)
        _ -> Nothing

    tryMoveR p = (p:) <$> case g M.!? (p + 2 * right) of
        Nothing -> Just []
        Just BoxL -> tryMoveR (p + 2 * right)
        _ -> Nothing

    tryMoveUD p = (p:) <$> case (g M.!? (p + d), g M.!? (p + d + right)) of
        (Nothing, Nothing) -> Just []
        (Nothing, Just BoxL) -> tryMoveUD (p + d + right)
        (Just BoxL, Just BoxR) -> tryMoveUD (p + d)
        (Just BoxR, Nothing) -> tryMoveUD (p + d + left)
        (Just BoxR, Just BoxL) -> (<>) <$> tryMoveUD (p + d + left) <*> tryMoveUD (p + d + right)
        _ -> Nothing


    shiftBoxes :: Point -> [Point] -> Grid WideCell -> Grid WideCell
    shiftBoxes d ps g = M.fromList ((,BoxL) <$> lefts) <> M.fromList ((,BoxR) <$> rights) <> M.withoutKeys g (S.fromList ps <> S.fromList ((+right) <$> ps)) where
      lefts = (+d) <$> ps 
      rights = (+right) . (+d) <$> ps

scoreWide :: Grid WideCell -> Int
scoreWide = getSum . M.foldMapWithKey (\(V2 x y) c -> case c of
  WWall -> 0
  BoxR -> 0
  BoxL -> Sum $ 100 * y + x)
