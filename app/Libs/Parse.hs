{-# LANGUAGE DerivingStrategies #-}

module Libs.Parse (
  Parser (..),
  char, newline, takeWhile1, spaces, take,
  decimal, hexadecimal, signed, rational, double,
) where

import Control.Applicative (Alternative (..), asum)
import Control.Monad ((<=<), void)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Read qualified as TR
import Prelude hiding (take)

newtype Parser a = P {runParser :: Text -> [(Text, a)]} deriving (Functor)

instance Applicative Parser where
  pure x = P $ \t -> [(t, x)]
  P pf <*> P px = P $ \t -> do
    (t', f) <- pf t
    fmap f <$> px t'

instance Alternative Parser where
  empty = P $ const []
  (<|>) :: Parser a -> Parser a -> Parser a
  P pa <|> P pb = P $ \t -> pa t ++ pb t

instance Monad Parser where
  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  pa >>= f = P $ uncurry (flip runParser) <=< runParser (f <$> pa)

-- combinators

-- text

char :: Char -> Parser Char
char c = P $ \t -> case T.uncons t of
  Just (x, t')
    | c == x -> [(t', c)]
  _ -> []

newline :: Parser ()
newline = void $ char '\n'

liftTextPair :: (Text -> (Text, Text)) -> Parser Text
liftTextPair f = P $ \t -> case f t of
  (l, r) -> [(l, r)]

liftTextPair1 :: (Text -> (Text, Text)) -> Parser Text
liftTextPair1 f = P $ \t -> case f t of
  (l, r)
    | T.null l -> empty
    | otherwise -> [(l, r)]

takeWhile1 :: (Char -> Bool) -> Parser Text
takeWhile1 p = liftTextPair1 $ T.span p

spaces :: Parser ()
spaces = void (takeWhile1 (== ' '))

take :: Int -> Parser Text
take i = liftTextPair $ T.splitAt i

-- numbers

liftTextReader :: TR.Reader a -> Parser a
liftTextReader r = P $ \t -> case r t of
  Left _ -> empty
  Right (x, t') -> [(t', x)]

decimal :: (Integral a) => Parser a
decimal = liftTextReader TR.decimal

hexadecimal :: (Integral a) => Parser a
hexadecimal = liftTextReader TR.hexadecimal

signed :: (Num a) => Parser a -> Parser a
signed p = do
  sign <- asum [char '-', char '+', pure '+']
  case sign of
    '-' -> negate <$> p
    _ -> p

rational :: (Fractional a) => Parser a
rational = liftTextReader TR.rational

double :: Parser Double
double = liftTextReader TR.double