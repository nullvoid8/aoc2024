{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Libs.Parse (Parser)
import Libs.Parse qualified as P
import Data.Text.IO qualified as TIO
import Data.Maybe (mapMaybe)

main :: IO ()
main = do
    input <- P.runOnceIO parser =<< TIO.readFile "input/day07"
    print $ sum . fmap fst . filter (uncurry (canAchieve p1Ops)) $ input
    print $ sum . fmap fst . filter (uncurry (canAchieve p2Ops)) $ input

parser :: Parser [(Int, [Int])]
parser = P.lines $ (,) <$> P.decimal <* P.text ": " <*> P.decimal `P.sepBy` P.spaces

data Op = Add | Mul | Cat deriving Show

p1Ops :: [Op]
p1Ops = [Add, Mul]

p2Ops :: [Op]
p2Ops = [Add, Mul, Cat]

uneval :: Op -> Int -> Int -> Maybe Int
uneval Add tgt x
  | tgt >= x = Just $ tgt - x
uneval Mul tgt x
  | tgt `rem` x == 0 = Just $ tgt `div` x
uneval Cat tgt x
  | x < 10 && tgt `rem` 10 == x = Just $ tgt `div` 10
  | x < 100 && tgt `rem` 100 == x = Just $ tgt `div` 100
  | x < 1000 && tgt `rem` 1000 == x = Just $ tgt `div` 1000
uneval _ _ _ = Nothing

canAchieve :: [Op] -> Int -> [Int] -> Bool
canAchieve ops goal xs = go (reverse xs) goal where
  go :: [Int] -> Int -> Bool
  go [] 0 = True
  go (x:xs) tgt = any (go xs) $ mapMaybe (\op -> uneval op tgt x) ops
  go _ _ = False
