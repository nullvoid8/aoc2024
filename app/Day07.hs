{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Libs.Parse (Parser)
import Libs.Parse qualified as P
import Data.Text.IO qualified as TIO

main :: IO ()
main = do
    input <- P.runOnceIO parser =<< TIO.readFile "input/day07"
    print $ sum . fmap fst . filter (\(g, xs) -> elem g $ allEvals p1Ops xs) $ input
    print $ sum . fmap fst . filter (\(g, xs) -> elem g $ allEvals p2Ops xs) $ input

parser :: Parser [(Int, [Int])]
parser = P.lines $ (,) <$> P.decimal <* P.text ": " <*> P.decimal `P.sepBy` P.spaces

allEvals :: [Op] -> [Int] -> [Int]
allEvals ops = go [] where
  go :: [Int] -> [Int] -> [Int]
  go !acc (x:y:xs) = foldr (\op acc -> go acc (eval op x y:xs)) acc ops
  go !acc [x] = x : acc
  go _ [] = error "ran out of numbers"


data Op = Add | Mul | Cat deriving Show

p1Ops :: [Op]
p1Ops = [Add, Mul]

p2Ops = [Add, Mul, Cat]

eval :: Op -> Int -> Int -> Int
eval Add x y = x + y
eval Mul x y = x * y
eval Cat x y = (x * (10 ^ length (show y))) + y