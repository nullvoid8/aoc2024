{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Main where

import Libs.Parse (Parser)
import Libs.Parse qualified as P
import Data.Text.IO qualified as TIO
import Data.IntMap (IntMap)
import Data.IntMap qualified as IM
import Data.IntSet (IntSet)
import Data.IntSet qualified as IS
import Data.Foldable (Foldable(..))
import Data.List (sortBy)

main :: IO ()
main = do
    (rules, books) <- P.runOnceIO parser =<< TIO.readFile "input/day05"
    print $ sum . fmap middle . filter (isSortedWith (ord rules)) $ books
    print $ sum . fmap (middle . sortBy (ord rules)) . filter (not . isSortedWith (ord rules)) $ books
  where
    middle :: Book -> Int
    middle xs = xs !! (length xs `quot` 2)

parser :: Parser (Rules, [Book])
parser = (,) <$> rules <* P.newline <*> books where
  rules :: Parser Rules
  rules = fmap fold . P.lines $ ruleSingleton <$> P.decimal <* P.char '|' <*> P.decimal
  books :: Parser [Book]
  books = P.lines $ P.decimal `P.sepBy` P.char ','

isSortedWith :: (Int -> Int -> Ordering) -> Book -> Bool
isSortedWith cmp xs = go xs (tail xs) where
  go (x:_) xs@(y:ys) = case cmp x y of
      GT -> False
      _ -> go xs ys
  go _ _ = True


type Book = [Int]
newtype Rules = R {unR :: IntMap (IntSet, IntSet)} deriving (Show)

instance Semigroup Rules where
  R x <> R y = R $ IM.unionWith (<>) x y

instance Monoid Rules where
  mempty :: Rules
  mempty = R IM.empty

-- x < y => y in xa && x in yb
ruleSingleton :: Int -> Int -> Rules
ruleSingleton x y = R $ IM.fromList [(x, (IS.empty, IS.singleton y)), (y, (IS.singleton x, IS.empty))]

lookupRule :: Rules -> Int -> (IntSet, IntSet)
lookupRule (R rules) k = IM.findWithDefault mempty k rules

ord :: Rules -> Int -> Int -> Ordering
ord rules = go where
  go :: Int -> Int -> Ordering
  go x y = case lookupRule rules x of
    (xb, xa)
      | y `IS.member` xb -> GT
      | y `IS.member` xa -> LT
      | otherwise -> EQ

