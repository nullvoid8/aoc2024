{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# LANGUAGE GADTs #-}

module Main (main) where

-- import Libs.Parse (Parser)
import Libs.Parse qualified as P
import Data.Text.IO qualified as TIO
import Data.Text (Text)
import Data.Text qualified as T
import Control.Applicative
import Data.Maybe (maybeToList, mapMaybe)
import qualified Data.Map as M
import Data.Monoid (Sum (..))

main :: IO ()
main = do
  (towels, designs) <- P.runOnceIO parser =<< TIO.readFile "input/day19"
  let p = designParser towels
  print $ length $ filter (canParse p) designs
  print $ sum $ fmap (countParse towels) designs
  -- print $ p2 input
  -- print $ memoTokens prog == memoTokensFast prog

type Input = ([Text], [Text])

parser :: P.Parser Input
parser = (,) <$> towels <* P.newline <* P.newline <*> designs where
  isColour :: Char -> Bool
  isColour x = x `elem` ("wubrg" :: String)
  towels = P.takeWhile1 isColour `P.sepBy` P.text ", "
  designs = P.takeWhile1 isColour `P.sepBy` P.newline

designParser :: [Text] -> Parser [Text]
designParser towels = some (asum $ fmap Match towels) <* EOF

-- GADT Parser

data Parser a where
  Fail :: Parser a
  FMap :: (a -> b) -> Parser a -> Parser b
  Pure :: forall a. a -> Parser a
  Ap :: forall a b. Parser (a -> b) -> Parser a -> Parser b
  Seq :: forall a b. Parser a -> Parser b -> Parser b
  RSeq :: forall a b. Parser a -> Parser b -> Parser a
  Alt :: forall a. Parser a -> Parser a -> Parser a
  Some :: forall a. Parser a -> Parser [a]
  Many :: forall a. Parser a -> Parser [a]
  Match :: Text -> Parser Text
  EOF :: Parser ()

instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap _ Fail = Fail
  fmap f (FMap g p) = FMap (f . g) p
  fmap f (Pure a) = Pure $ f a
  fmap f (Ap p q) = FMap f $ Ap p q
  fmap f (Seq p q) = Seq p (fmap f q)
  fmap f (RSeq p q) = RSeq (fmap f p) q
  fmap f (Alt p q) = Alt (fmap f p) (fmap f q)
  fmap f p = FMap f p

instance Applicative Parser where
  pure :: a -> Parser a
  pure = Pure
  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  (<*>) = Ap
  (*>) :: Parser a -> Parser b -> Parser b
  (*>) = Seq
  (<*) :: Parser a -> Parser b -> Parser a
  (<*) = RSeq

instance Alternative Parser where
  empty :: Parser a
  empty = Fail
  (<|>) :: Parser a -> Parser a -> Parser a
  (<|>) = Alt
  some :: Parser a -> Parser [a]
  some = Some
  many :: Parser a -> Parser [a]
  many = Many

instance Show (Parser a) where
  show Fail = "Fail"
  show (FMap _ p) = "FMap _ (" <> show p <> ")"
  show (Pure _) = "Pure _"
  show (Ap p q) = "Ap (" <> show p <> ") (" <> show q <> ")"
  show (Seq p q) = "Seq (" <> show p <> ") (" <> show q <> ")"
  show (RSeq p q) = "RSeq (" <> show p <> ") (" <> show q <> ")"
  show (Alt p q) = "Alt (" <> show p <> ") (" <> show q <> ")"
  show (Some p) = "Some (" <> show p <> ")"
  show (Many p) = "Many (" <> show p <> ")"
  show (Match t) = "Match " <> T.unpack t
  show EOF = "EOF"

canParse :: Parser a -> Text -> Bool
canParse p t = not . null $ go p t where
  go :: Parser a -> Text -> [Text]
  go Fail _ = []
  go (FMap _ p) t = go p t
  go (Pure _) t = [t]
  go (Ap p q) t = go p t >>= go q
  go (Seq p q) t = go p t >>= go q
  go (RSeq p q) t = go p t >>= go q
  go (Alt p q) t = go p t <|> go q t
  go (Some p) t = go p t >>= go (Many p)
  go (Many p) t = go (Some p) t <|> [t]
  go (Match s) t = maybeToList $ T.stripPrefix s t
  go EOF t = [t | T.null t]

countParse :: [Text] -> Text -> Int
countParse towels design = getSum $ go design where
  memo :: M.Map Text (Sum Int)
  memo = M.fromList ((\t -> (t, go t)) <$> T.tails design )

  go :: Text -> Sum Int
  go t
    | T.null t = 1
    | otherwise = foldMap (memo M.!) . mapMaybe (`T.stripPrefix` t) $ towels

