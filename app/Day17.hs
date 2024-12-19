{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Main (main) where

import Libs.Parse (Parser)
import Libs.Parse qualified as P
import Data.Text.IO qualified as TIO
import Data.Text (Text)
import Control.Applicative (optional)
import Data.Bits
import Control.Monad.Trans.State.Strict
import Data.Int (Int64, Int16)
import qualified Data.IntMap as M
import Data.Maybe (fromMaybe)
import Debug.Trace (traceShowId)

main :: IO ()
main = do
  (st, prog) <- P.runOnceIO parser =<< TIO.readFile "input/day17"
  -- print $ run prog st
  print $ encrypt prog
  -- print $ memoTokens prog == memoTokensFast prog

parser :: Parser (ProgState, Program)
parser = (,) <$> state <* P.newline <*> program where
  state :: Parser ProgState
  state = S 0 <$> kv "Register A" P.decimal <*> kv "Register B" P.decimal <*> kv "Register C" P.decimal
  program = kv "Program" $ P.decimal `P.sepBy` P.char ','

  kv :: Text -> Parser a -> Parser a
  kv key value = P.text key *> P.text ": " *> value <* optional P.newline

data Op = ADV Combo | BXL Literal | BST Combo | JNZ Literal | BXC | OUT Combo | BDV Combo | CDV Combo deriving Show
data Combo = Zero | One | Two | Three | REGA | REGB | REGC | NOP deriving Show
type Literal = Int

data ProgState = S {ip, rega, regb, regc :: Int} deriving Show
type Program = [Int]

deref :: Combo -> ProgState -> Int
deref Zero _ = 0
deref One _ = 1
deref Two _ = 2
deref Three _ = 3
deref REGA st = rega st
deref REGB st = regb st
deref REGC st = regc st
deref NOP _ = error "unreachable"

exec :: Op -> ProgState -> ([Int], ProgState)
exec (ADV op) st = ([], st{ip = ip st + 2, rega = rega st `div` (2 ^ deref op st)})
exec (BXL op) st = ([], st{ip = ip st + 2, regb = regb st `xor` op})
exec (BST op) st = ([], st{ip = ip st + 2, regb = deref op st `mod` 8})
exec (JNZ op) st = let ra = rega st in  ([], st{ip = if ra == 0 then ip st + 2 else op})
exec  BXC st = ([], st{ip = ip st + 2, regb = regb st `xor` regc st})
exec (OUT op) st = ([deref op st `mod` 8], st{ip = ip st + 2})
exec (BDV op) st = ([], st{ip = ip st + 2, regb = rega st `div` (2 ^ deref op st)})
exec (CDV op) st = ([], st{ip = ip st + 2, regc = rega st `div` (2 ^ deref op st)})

parseCombo :: Int -> Combo
parseCombo 0 = Zero
parseCombo 1 = One
parseCombo 2 = Two
parseCombo 3 = Three
parseCombo 4 = REGA
parseCombo 5 = REGB
parseCombo 6 = REGC
parseCombo 7 = NOP
parseCombo _ = error "unreachable"

parseOp :: Int -> Int -> Op
parseOp 0 x = ADV $ parseCombo x
parseOp 1 x = BXL x
parseOp 2 x = BST $ parseCombo x
parseOp 3 x = JNZ x
parseOp 4 _ = BXC
parseOp 5 x = OUT $ parseCombo x
parseOp 6 x = BDV $ parseCombo x
parseOp 7 x = CDV $ parseCombo x
parseOp _ _ = error "unreachable"


run :: Program -> ProgState -> [Int]
run prog = evalState go where
  maxIP = length prog
  parseInstr :: Int -> Maybe Op
  parseInstr ip
    | ip >= maxIP = Nothing
    | otherwise = Just $ parseOp (prog !! ip) (prog !! (ip + 1))

  go :: State ProgState [Int]
  go = do
    op <- parseInstr . ip <$> get
    case op of
      Nothing -> pure []
      Just op -> (<>) <$> state (exec op) <*> go

runFast :: Program -> ProgState -> [Int]
runFast _ (S _ 0 _ _ ) = [5]
runFast _ (S _ rega _ _ ) = go rega where
  go 0 = []
  go x = ((((x .&. 0b111) .^. 2) .^. (x .>>. ((x .&. 0b111) .^. 2))) .^. 7) .&. 0b111 : go (x .>>. 3)


-- $ fmap (\(x,k) -> (k, [x])) [(5,0),(13,0),(21,0),(29,0),(33,0),(37,0),(45,0),(53,0),(54,0),(61,0),(62,0),(68,0),(69,0),(71,0),(76,0),(77,0),(79,0),(84,0),(85,0),(87,0),(92,0),(93,0),(95,0),(97,0),(100,0),(101,0),(108,0),(109,0),(116,0),(117,0),(124,0),(125,0),(4,1),(12,1),(16,1),(20,1),(28,1),(36,1),(38,1),(41,1),(44,1),(46,1),(48,1),(52,1),(60,1),(80,1),(103,1),(105,1),(111,1),(112,1),(119,1),(127,1),(7,2),(15,2),(22,2),(23,2),(30,2),(31,2),(49,2),(113,2),(6,3),(11,3),(14,3),(24,3),(27,3),(39,3),(43,3),(47,3),(55,3),(56,3),(57,3),(59,3),(63,3),(75,3),(88,3),(91,3),(107,3),(120,3),(121,3),(123,3),(1,4),(65,4),(118,4),(126,4),(0,5),(2,5),(9,5),(10,5),(18,5),(26,5),(32,5),(34,5),(42,5),(50,5),(58,5),(64,5),(66,5),(73,5),(74,5),(82,5),(90,5),(96,5),(98,5),(102,5),(106,5),(110,5),(114,5),(122,5),(17,6),(81,6),(86,6),(94,6),(3,7),(8,7),(19,7),(25,7),(35,7),(40,7),(51,7),(67,7),(70,7),(72,7),(78,7),(83,7),(89,7),(99,7),(104,7),(115,7)]

-- prev    hgfedcba
--    x hgfedcba

memoTokens :: Program -> M.IntMap [Int64]
memoTokens prog = M.fromListWith (<>) $ do
    x <- ([1..] :: [Int16])
    let out = head $ run prog (S 0 (fromIntegral x) 0 0)
    pure (out, [fromIntegral x])

memoTokensFast :: Program -> M.IntMap [Int64]
memoTokensFast prog = M.fromListWith (<>) $ do
    x <- ([1..] :: [Int16])
    let out = head $ runFast prog (S 0 (fromIntegral x) 0 0)
    pure (out, [fromIntegral x])

encrypt :: Program -> [Int64]
encrypt prog = filter (\rega -> run prog (S 0 (fromIntegral rega) 0 0) == [7,1,3,7,5,1,0,3,4]) $ go Nothing [7,1,3,7,5,1,0,3,4] where
  tokens = memoTokens prog

  nextToken :: Int -> Int64 -> [Int64]
  nextToken target prev = filter (\x -> ((x .^. (prev .>>. 3)) .&. 8191)== 0) $ fromMaybe [] $ tokens M.!? target

  go :: Maybe Int64 -> [Int] -> [Int64]
  go _ [] = [0]
  go Nothing (x:xs) = do
    token <- tokens M.! x
    rest <- go (Just token) xs
    pure $ token .|. (rest .<<. 3)
  go (Just prev) (x:xs) = do
    token <- nextToken x prev
    rest <- go (Just token) xs
    pure $ token .|. (rest .<<. 3)
