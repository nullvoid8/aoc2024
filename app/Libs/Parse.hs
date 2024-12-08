{-# LANGUAGE DerivingStrategies, OverloadedStrings, ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Libs.Parse (
  Parser (..),
  char, newline, takeWhile1, spaces, take,
  decimal, hexadecimal, signed, rational, double, runOnce, sepBy, lines, anychar, runOnceIO, skip, text, limit, grid, satisfies
) where

import Control.Applicative (Alternative (..), optional)
import Data.Functor (void)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Read qualified as TR
import Prelude hiding (take, lines)
import qualified Data.Map as M
import Libs.V2 ( V2(..) )
import Data.Foldable
import Data.Semigroup (Min (..), Max (..))
import Data.Coerce (coerce)

data State = St !Int !Text deriving Show

instance Semigroup State where
  St a _ <> St b t = St (a + b) t

consumed :: State -> Int
consumed (St c _) = c

rest :: State -> Text
rest (St _ t) = t

data Result a =
  Fail !Text | Success !State !a (Result a) deriving (Functor, Show)

instance Semigroup (Result a) where
  (<>) :: Result a -> Result a -> Result a
  Fail xs <> Fail _ = Fail xs
  Fail _ <> xs = xs
  Success s x l <> r = Success s x $ l <> r

mergeState :: State -> Result a -> Result a
mergeState st = go where
  go (Success st' a r) = Success (st <> st') a $ go r
  go x = x

newtype Parser a = P {runParser :: Text -> Result a} deriving (Functor)

runOnce :: Parser a -> Text -> Either Text a
runOnce p t = case runParser p t of
  Fail err -> Left err
  Success _ x _ -> Right x

runOnceIO :: Parser a -> Text -> IO a
runOnceIO p t = case runOnce p t of
  Left err -> fail $ show err
  Right r -> pure r

instance Applicative Parser where
  pure :: a -> Parser a
  pure x = P $ \t -> Success (St 0 t) x $ Fail "pure"

  (<*>) :: forall a b. Parser (a -> b) -> Parser a -> Parser b
  pf <*> px = P $ \t -> go $ runParser pf t
    where
      go :: Result (a -> b) -> Result b
      go (Fail err) = Fail err
      go (Success st f fs) = (f <$> mergeState st (runParser px $ rest st)) <> go fs

instance Alternative Parser where
  empty :: Parser a
  empty = P $ const $ Fail "empty"

  (<|>) :: Parser a -> Parser a -> Parser a
  pa <|> pb = P $ \t -> runParser pa t <> runParser pb t

instance Monad Parser where
  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  pa >>= f = P $ \t -> go $ runParser pa t  where
    go (Success s x r) = mergeState s (runParser (f x) (rest s)) <> go r
    go (Fail err) = Fail err


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
    go :: Int -> Text -> Result ()
    go !l str
      | T.null str = Success (St l str) () $ Fail "EOF"
      | otherwise =  Success (St l str) () $ go (l+1) (T.tail str)

limit :: Int -> Parser a -> Parser a
limit l p = P $ go . runParser p where
  go :: Result a -> Result a
  go (Success st x r)
    | consumed st <= l = Success st x $ go r
    | otherwise = go r
  go x = x

grid :: forall row a. Parser row -> Parser (Maybe a) -> Parser (V2 Int, V2 Int, M.Map (V2 Int) a)
grid rowSep cell = let
  singleRow :: Parser (Int -> (V2 (Min Int), V2 (Max Int), M.Map (V2 Int) a))
  singleRow = fold . zipWith (\ c x r -> let p = V2 c r in  (Min <$> p, Max <$> p, maybe M.empty (M.singleton p) x) ) [0..] <$> some cell
  wholeGrid :: Parser (V2 Int, V2 Int, M.Map (V2 Int) a)
  wholeGrid = coerce . fold . zipWith (\r f -> f r) [0..] <$> singleRow `sepBy` rowSep
  in wholeGrid
-- text

anychar :: Parser Char
anychar = P $ \t -> case T.uncons t of
  Just (c, t') -> Success (St 1 t') c $ Fail "anychar"
  Nothing -> Fail "anychar: unexpected EOF"

char :: Char -> Parser Char
char c = P $ \t -> case T.uncons t of
  Just (x, t')
    | c == x -> Success (St 1 t') x $ Fail "char"
    | otherwise -> Fail $ "expected '" <> T.pack (show c) <> "' got EOF" <> T.pack (show x) <> "'"
  _ -> Fail $ "expected '" <> T.pack (show c) <> "' got EOF"

satisfies :: (Char -> Bool) -> Parser Char
satisfies f = P $ \t -> case T.uncons t of
  Just (x, t')
    | f x -> Success (St 1 t') x $ Fail "char"
    | otherwise -> Fail $ "failed to satisfy with '" <> T.singleton x <> "'"
  _ -> Fail "failed to satisfy with EOF"

text :: Text -> Parser Text
text prefix = P $ \t -> case T.stripPrefix prefix t of
  Nothing -> Fail $ "expected prefix '" <> prefix <> "' got '" <> T.take (T.length prefix) t <> "'"
  Just rest -> Success (St (T.length prefix) rest) prefix $ Fail "prefix"

newline :: Parser ()
newline = void $ char '\n'

liftTextPair :: (Text -> (Text, Text)) -> Parser Text
liftTextPair f = P $ \t -> case f t of
  (l, r) -> Success (St (T.length l) r) l $ Fail "liftTextPair"

liftTextPair1 :: (Text -> (Text, Text)) -> Parser Text
liftTextPair1 f = P $ \t -> case f t of
  (l, r)
    | T.null l -> Fail "liftTextPair1"
    | otherwise -> Success (St (T.length l) r) l $ Fail "liftTextPair1"

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
  Right (x, t') -> Success (St (T.length t - T.length t') t') x $ Fail "liftTextReader"

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