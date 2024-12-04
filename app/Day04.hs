{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import  Data.Text (Text)
import Data.Semigroup (Sum (getSum, Sum))

main :: IO ()
main = do
  input <- T.lines <$> TIO.readFile "input/day04"

  let rows = getSum . foldMap (Sum . countXmas) $ input
  let cols = getSum . foldMap (Sum . countXmas) $ T.transpose input
  let pos = getSum . foldMap (Sum . countXmas) $ posDiags input
  let neg = getSum . foldMap (Sum . countXmas) $ negDiags input

  print $ rows + cols + pos + neg

  print $ p2 input
  -- print $ getSum $ rows + cols

countXmas :: Text -> Int
countXmas = getSum <$> (Sum . T.count "XMAS") <> (Sum . T.count "SAMX")


posDiags, negDiags :: [Text] -> [Text]
posDiags [] = []
posDiags xs = T.transpose $ zipWith3 (\a b t -> a <> t <> b) before after xs where
  len = T.length (head xs)
  before = T.tails $ T.replicate len "."
  after = T.inits $ T.replicate len "."

negDiags [] = []
negDiags xs = T.transpose $ zipWith3 (\a b t -> a <> t <> b) before after xs where
  len = T.length (head xs)
  before = T.inits $ T.replicate len "."
  after = T.tails $ T.replicate len "."

p2 :: [Text] -> Int
p2 = go 0 where
  go :: Int -> [Text] -> Int
  go !acc (top:xs@(mid:bot:_)) = go (acc + go' top mid bot) xs
  go !acc _ = acc

  go' :: Text -> Text -> Text -> Int
  go' top mid bot = sum $ zipWith3 match (T.unpack . T.take 3 <$> T.tails top) (drop 1 . T.unpack $ mid) (T.unpack . T.take 3 <$> T.tails bot)

  match :: String -> Char -> String -> Int
  match ('M':_:'M':_) 'A' ('S':_:'S':_) = 1
  match ('M':_:'S':_) 'A' ('M':_:'S':_) = 1
  match ('S':_:'M':_) 'A' ('S':_:'M':_) = 1
  match ('S':_:'S':_) 'A' ('M':_:'M':_) = 1
  match _ _ _ = 0
