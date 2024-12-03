{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import qualified Data.Text.IO as TIO
import qualified Libs.Parse as P
import Control.Applicative (optional, asum)

main :: IO ()
main = do
  input <- P.runOnceIO parser =<< TIO.readFile "input/day03"
  print $ calcP1 input
  print $ calcP2 input

calcP1 :: [Instruction] -> Int
calcP1 = sum . fmap (\case
  Mul x y -> x * y
  _ -> 0)

calcP2 :: [Instruction] -> Int
calcP2 = go 0 True where
  go !acc _ [] = acc
  go !acc True (Mul x y : xs) = go (acc + x*y) True xs
  go !acc False (Do : xs) = go acc True xs
  go !acc True (Dont : xs) = go acc False xs
  go !acc !enabled (_:xs) = go acc enabled xs


data Instruction = Mul Int Int | Do | Dont deriving Show 

parser :: P.Parser [Instruction]
parser = optional P.skip *> instruction `P.sepBy` P.skip
  where
    instruction = asum [mul, doo, dont]
    mul = Mul <$ P.text "mul(" <*> P.limit 3 P.decimal <* P.char ',' <*> P.limit 3 P.decimal <* P.char ')'
    doo = Do <$ P.text "do()"
    dont = Dont <$ P.text "don't()"
