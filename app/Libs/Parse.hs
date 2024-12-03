{-# LANGUAGE DerivingStrategies, OverloadedStrings, ScopedTypeVariables #-}

module Libs.Parse (
  Parser (..),
  char, newline, takeWhile1, spaces, take,
  decimal, hexadecimal, signed, rational, double, runOnce, sepBy, lines, anychar, runOnceIO, skip, text, limit
) where

import Control.Applicative (Alternative (..), asum, optional)
import Data.Functor (void)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Read qualified as TR
import Prelude hiding (take, lines)
import Data.Semigroup (Sum (getSum, Sum))

type State = Sum Int

getConsumed :: State -> Int
getConsumed = getSum

data Result a = 
  Fail !Text | Success !State !Text !a (Result a) deriving (Functor, Show)

instance Semigroup (Result a) where
  (<>) :: Result a -> Result a -> Result a
  Fail xs <> Fail _ = Fail xs
  Fail _ <> xs = xs
  Success d t x l <> r = Success d t x $ l <> r

-- instance Monoid (Result a) where
--   mempty :: Result a
--   mempty = Fail "mempty"

newtype Parser a = P {runParser :: Text -> Result a} deriving (Functor)

runOnce :: Parser a -> Text -> Either Text a
runOnce p t = case runParser p t of
  Fail err -> Left err
  Success _ _ x _ -> Right x

runOnceIO :: Parser a -> Text -> IO a
runOnceIO p t = case runOnce p t of
  Left err -> fail $ show err
  Right r -> pure r

instance Applicative Parser where
  pure :: a -> Parser a
  pure x = P $ \t -> Success 0 t x $ Fail "pure"

  (<*>) :: forall a b. Parser (a -> b) -> Parser a -> Parser b
  pf <*> px = P $ \t -> go $ runParser pf t
    where
      go :: Result (a -> b) -> Result b
      go (Fail err) = Fail err
      go (Success d t f fs) = (f <$> withState d (runParser px t)) <> go fs

      withState :: State -> Result c -> Result c
      withState s (Success d t x r) = Success (s <> d) t x $ withState s r
      withState _ x = x


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

sepBy :: Parser a -> Parser sep -> Parser [a]
sepBy p sep = (:) <$> p <*> some (sep *> p) <* optional sep

lines :: Parser a -> Parser [a]
lines p = p `sepBy` newline

skip :: Parser ()
skip = P $ go 0
  where
    go :: Sum Int -> Text -> Result ()
    go !l str
      | T.null str = Success l str () $ Fail "EOF"
      | otherwise =  Success l str () $ go (l+1) (T.tail str)

limit :: Int -> Parser a -> Parser a
limit l p = P $ go . runParser p where
  go :: Result a -> Result a
  go (Success d t x r)
    | getConsumed d <= l = Success d t x $ go r
    | otherwise = go r
  go x = x

-- text

anychar :: Parser Char
anychar = P $ \t -> case T.uncons t of
  Just (c, t') -> Success 1 t' c $ Fail "anychar"
  Nothing -> Fail "anychar: unexpected EOF"

char :: Char -> Parser Char
char c = P $ \t -> case T.uncons t of
  Just (x, t')
    | c == x -> Success 1 t' x $ Fail "char"
    | otherwise -> Fail $ "expected '" <> T.pack (show c) <> "' got EOF" <> T.pack (show x) <> "'"
  _ -> Fail $ "expected '" <> T.pack (show c) <> "' got EOF"

text :: Text -> Parser Text
text prefix = P $ \t -> case T.stripPrefix prefix t of
  Nothing -> Fail $ "expected prefix '" <> prefix <> "' got '" <> T.take (T.length prefix) t <> "'"
  Just rest -> Success (Sum $ T.length prefix) rest prefix $ Fail "prefix"

newline :: Parser ()
newline = void $ char '\n'

liftTextPair :: (Text -> (Text, Text)) -> Parser Text
liftTextPair f = P $ \t -> case f t of
  (l, r) -> Success (Sum $ T.length l) r l $ Fail "liftTextPair"

liftTextPair1 :: (Text -> (Text, Text)) -> Parser Text
liftTextPair1 f = P $ \t -> case f t of
  (l, r)
    | T.null l -> Fail "liftTextPair1"
    | otherwise -> Success  (Sum $ T.length l) r l $ Fail "liftTextPair1"

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
  Right (x, t') -> Success  (Sum $ T.length t - T.length t') t' x $ Fail "liftTextReader"

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