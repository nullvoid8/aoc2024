{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Main where

import Libs.Parse (Parser)
import Libs.Parse qualified as P
import Data.Text.IO qualified as TIO
import Libs.V2

main :: IO ()
main = do
  input <- P.runOnceIO parser =<< TIO.readFile "input/day13"
  print $ sum $ fmap scoreP1 input
  print $ sum $ fmap scoreP2 input


data Machine = Machine {buttonA, buttonB, prize :: V2 Int} deriving Show

parser :: Parser [Machine]
parser = (Machine <$> button <* P.newline <*> button <* P.newline <*> prize ) `P.sepBy` (P.newline *> P.newline) where
  button, prize :: Parser (V2 Int)
  -- Button A: X+94, Y+34
  button = V2 <$ P.text "Button " <* P.anychar <* P.text ": X" <*> P.signed P.decimal <* P.text ", Y" <*> P.signed P.decimal
  -- Prize: X=8400, Y=5400
  prize = V2 <$ P.text "Prize: X=" <*> P.signed P.decimal <* P.text ", Y=" <*> P.signed P.decimal


scoreP1 :: Machine -> Int
scoreP1 (Machine (V2 ax ay) (V2 bx by) (V2 px py))
  | modA == 0 && modB == 0 = 3 *  divA + divB
  | otherwise = 0
  where V2 (divA, modA) (divB, modB) = divMod <$> V2 (by*px - bx*py) (ax*py-ay*px) <*> pure (ax*by - bx*ay)

scoreP2 :: Machine -> Int
scoreP2 (Machine btnA btnB prize) = scoreP1 $ Machine btnA btnB (prize + 10000000000000)

--          x
--          y
--        ---
-- ax bx | px
-- ay by | py

--           px
--           py
--          ---
--  by -bx |  x
-- -ay  ax |  y