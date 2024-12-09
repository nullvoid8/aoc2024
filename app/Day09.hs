{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Main where

import Libs.Parse (Parser)
import Libs.Parse qualified as P
import Data.Text.IO qualified as TIO
import Data.Text.Read qualified as TR
import Data.Text qualified as T
import Data.Char (isDigit)
import Control.Applicative
import Data.Semigroup (Max(..))
import Data.Coerce (coerce)

main :: IO ()
main = do
  (maxFileID, input) <- P.runOnceIO parser =<< TIO.readFile "input/day09"
  print $ checksum $ simpleCompression input
  print $ checksum $ smartCompression maxFileID input

data File = File Int Int deriving Show

type FS = Either File Int

parser :: Parser (Int, [FS])
parser =  fmap (coerce . go 0) . some $ (\(Right (x, _)) -> x) . TR.decimal . T.singleton <$> P.satisfies isDigit where
  go, goFile, goFree :: Int -> [Int] -> (Max Int, [FS])
  go = goFile
  goFile _ [] = (Max 0, [])
  goFile fileID (x:xs) = (Max fileID, [Left (File fileID x)]) <> goFree (fileID + 1) xs
  goFree _ [] = (Max 0, [])
  goFree fileID (x:xs) = (Max 0, [Right x]) <> goFile fileID xs

checksum :: [FS] -> Int
checksum = go 0 0 where
  go :: Int -> Int -> [FS] -> Int
  go !acc _ [] = acc
  go !acc !start (Right len : xs) = go acc (start + len) xs
  --  sum [start..len] = tri(start+len) - tri(start). minus 1 to both because zero indexing
  go !acc !start (Left (File id len) : xs) = let sum = id * len * (len + 2 * start - 1) `div` 2 in go (acc + sum) (start + len) xs

pickLastFile :: [FS] -> Maybe (File, [FS])
pickLastFile = go where
  go :: [FS] -> Maybe (File, [FS])
  go [] = Nothing
  go [Left file] = Just (file, [])
  go [Right _, Left file] = Just (file, [])
  go (x:xs) = fmap (x:) <$> go xs

simpleCompression :: [FS] -> [FS]
simpleCompression xs = case pickLastFile xs of
    Nothing -> []
    Just (f, xs) -> go f xs
  where
    go :: File -> [FS] -> [FS]
    go file [] = [Left file]
    go file (Left x : xs) = Left x : go file xs
    go (File id len) (Right free : xs) = case compare len free of
      LT -> Left (File id len) : simpleCompression (Right (free - len) : xs)
      EQ -> Left (File id len) : simpleCompression xs
      GT -> Left (File id free) : go (File id (len - free)) xs

showFS :: [FS] -> T.Text
showFS (Left (File id len) : xs) = T.replicate len (T.pack $ show id) <> showFS xs
showFS (Right free : xs) = T.replicate free "." <> showFS xs
showFS [] = ""

-- remove a file from the FS, replacing it with Free Space
pickFile :: Int -> [FS] -> Maybe (File, [FS])
pickFile _ [] = Nothing
pickFile tgt (Left f@(File id len) : xs)
  | tgt == id = Just (f, normalise $ Right len : xs)
  where
    normalise :: [FS] -> [FS]
    normalise [Right _] = []
    normalise (Right x : Right y : xs) = normalise $ Right (x + y) : xs
    normalise xs = xs
pickFile tgt (x:xs) = fmap (x:) <$> pickFile tgt xs

smartCompression :: Int -> [FS] -> [FS]
smartCompression !tgt fs = case pickFile tgt fs of
    Nothing -> fs
    Just (file, fs) -> smartCompression (tgt - 1) $ go file fs
  where
    go :: File -> [FS] -> [FS]
    go file [] = [Left file]
    go file@(File _ len) (Right free : xs) = case compare len free of
      LT -> Left file : Right (free - len) : xs
      EQ -> Left file : xs
      GT -> Right free : go file xs
    go file (x:xs) = x : go file xs

