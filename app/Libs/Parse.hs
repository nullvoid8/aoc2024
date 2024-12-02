{-# LANGUAGE DerivingStrategies, OverloadedStrings, ScopedTypeVariables #-}

module Libs.Parse (
  Parser (..),
  char, newline, takeWhile1, spaces, take,
  decimal, hexadecimal, signed, rational, double, runOnce,
) where

import Control.Applicative (Alternative (..), asum)
import Data.Functor (void)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Read qualified as TR
import Prelude hiding (take)

data Result a = Fail Text | Success Text a (Result a) deriving (Functor, Show)

instance Semigroup (Result a) where
  (<>) :: Result a -> Result a -> Result a
  Fail xs <> Fail _ = Fail xs
  Fail _ <> xs = xs
  Success t x l <> r = Success t x $ l <> r

-- instance Monoid (Result a) where
--   mempty :: Result a
--   mempty = Fail "mempty"

newtype Parser a = P {runParser :: Text -> Result a} deriving (Functor)

runOnce :: Parser a -> Text -> Either Text a
runOnce p t = case runParser p t of
  Fail err -> Left err
  Success _ x _ -> Right x

instance Applicative Parser where
  pure :: a -> Parser a
  pure x = P $ \t -> Success t x $ Fail "pure"
  
  (<*>) :: forall a b. Parser (a -> b) -> Parser a -> Parser b
  pf <*> px = P $ \t -> go $ runParser pf t
    where
      go :: Result (a -> b) -> Result b
      go (Fail err) = Fail err
      go (Success t f fs) = (f <$> runParser px t) <> go fs

instance Alternative Parser where
  empty :: Parser a
  empty = P $ const $ Fail "empty"
  
  (<|>) :: Parser a -> Parser a -> Parser a
  pa <|> pb = P $ \t -> runParser pa t <> runParser pb t

-- combinators

failWith :: Text -> Parser a -> Parser a
failWith msg (P p) = P $ \t -> case p t of
  (Fail _) -> Fail msg
  rs -> rs

-- text

char :: Char -> Parser Char
char c = P $ \t -> case T.uncons t of
  Just (x, t')
    | c == x -> Success t' x $ Fail "char"
    | otherwise -> Fail $ "expected '" <> T.pack (show c) <> "' got EOF" <> T.pack (show x) <> "'"
  _ -> Fail $ "expected '" <> T.pack (show c) <> "' got EOF"

newline :: Parser ()
newline = void $ char '\n'

liftTextPair :: (Text -> (Text, Text)) -> Parser Text
liftTextPair f = P $ \t -> case f t of
  (l, r) -> Success r l $ Fail "liftTextPair"

liftTextPair1 :: (Text -> (Text, Text)) -> Parser Text
liftTextPair1 f = P $ \t -> case f t of
  (l, r)
    | T.null l -> Fail "liftTextPair1"
    | otherwise -> Success r l $ Fail "liftTextPair1"

takeWhile1 :: (Char -> Bool) -> Parser Text
takeWhile1 p = failWith "takeWhile1"$ liftTextPair1 $ T.span p

spaces :: Parser ()
spaces = void (takeWhile1 (== ' '))

take :: Int -> Parser Text
take i = liftTextPair $ T.splitAt i

-- numbers

liftTextReader :: TR.Reader a -> Parser a
liftTextReader r = P $ \t -> case r t of
  Left err -> Fail $ T.pack err
  Right (x, t') -> Success t' x $ Fail "liftTextReader"

decimal :: (Integral a) => Parser a
decimal = liftTextReader TR.decimal

hexadecimal :: (Integral a) => Parser a
hexadecimal = liftTextReader TR.hexadecimal

signed :: (Num a) => Parser a -> Parser a
signed p = signer <$> asum [char '-', char '+', pure '+'] <*> p
  where
    signer '-' x = negate x
    signer _ x = x

rational :: (Fractional a) => Parser a
rational = liftTextReader TR.rational

double :: Parser Double
double = liftTextReader TR.double