{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# LANGUAGE GADTs #-}

module Main (main) where

-- import Libs.Parse (Parser)
import Libs.Parse qualified as P
import Data.Text.IO qualified as TIO
import Data.Int (Int64)
import Data.Bits
import qualified Data.Map as M
import Data.List ( tails )
import Data.Monoid (Last (..))
import Data.Foldable (Foldable(..))

main :: IO ()
main = do
  input <- P.runOnceIO parser =<< TIO.readFile "input/day22"
  print $ sum $ fmap ((!! 2000) . gen) input
  print $ maximum $ M.unionsWith (+) $ fmap pricing input

parser :: P.Parser [Int64]
parser = P.lines P.decimal

gen :: Int64 -> [Int64]
gen = iterate go where
  go s = c where
    a = ((s .<<.  6) .^. s) .&. 0b1111_1111_1111_1111_1111_1111
    b = ((a .>>.  5) .^. a) .&. 0b1111_1111_1111_1111_1111_1111
    c = ((b .<<. 11) .^. b) .&. 0b1111_1111_1111_1111_1111_1111
        
sliding :: Int -> [a] -> [[a]]
sliding len xs = filter ((== len) . length) . fmap (take len) $ tails xs

-- Turn a seed price into a map of {[4 differences]: final price}
pricing :: Int64 -> M.Map [Int64] Int64
pricing seed = foldMap ((\(ds, Last (Just p)) -> M.singleton ds p) . fold) . sliding 4 . (zipWith (\x y -> ([y-x], Last . Just $ y)) <*> tail) . fmap (`mod` 10) . take 2001 $ gen seed
