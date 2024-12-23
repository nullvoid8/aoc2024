{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE OverloadedStrings, OverloadedLists #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# LANGUAGE GADTs #-}

module Main (main) where

-- import Libs.Parse (Parser)
import Libs.Parse qualified as P
import Data.Text.IO qualified as TIO
import Data.Set (Set)
import Data.Text (Text)
import Data.Char (isAlpha)
import qualified Data.Set as S
import Control.Monad (guard)
import qualified Data.Text as T
import Data.Maybe (isJust)
import qualified Data.Map as M
import Data.Map (Map)
import Debug.Trace (traceShowId, trace)

main :: IO ()
main = do
  input <- P.runOnceIO parser =<< TIO.readFile "input/day23"
  -- print input
  print $ p1 input
  print $ p2 input
  

type Node = Text
type Edge = Set Node

parser :: P.Parser [Edge]
parser = P.lines $ (\x y -> S.fromList [x,y]) <$> P.takeWhile1 isAlpha <* P.char '-' <*> P.takeWhile1 isAlpha 

p1 :: [Edge] -> Int
-- p1 edges = 
p1 edges = S.size . S.filter (not . S.null . S.intersection tNodes) $ cliques edges !! 2 where
  tNodes :: Set Node
  tNodes = S.filter (isJust . T.stripPrefix "t") $ S.unions edges

p2 :: [Edge] -> Text
p2 edges = case largest of
    [[xs]] -> showClique xs
    xs -> "error " <> T.pack (show xs)
  where
    largest :: [Set (Set Node)]
    largest = take 1 $ dropWhile (\xs -> length xs > 1) $ cliques edges

    showClique :: Set Node -> Text
    showClique = T.intercalate "," . S.toAscList
  
cliques :: [Edge] -> [Set (Set Node)]
cliques edges = nodes : iterate cliqueNP1 (S.fromList edges) where
  nodes :: Set (Set Node)
  nodes = S.fromList . fmap S.singleton . S.toList $ S.unions edges

  connections :: Map Node (Set Node)
  connections = M.unionsWith (<>) . fmap toAssoc $ edges

  toAssoc :: Edge -> Map Node (Set Node)
  toAssoc edge = [(a, [b]), (b, [a])] where
    a = S.elemAt 0 edge
    b = S.elemAt 1 edge
  
  cliqueNP1 :: Set (Set Node) -> Set (Set Node)
  cliqueNP1 cliques = S.fromList $ do
    c <- S.toList cliques
    n <- M.keys $ M.filter (S.isSubsetOf c) connections
    guard $ n `S.notMember` c
    pure $ S.insert n c

