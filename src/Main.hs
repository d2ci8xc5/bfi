module Main where

import System.IO (hFlush, stdout)
import Data.Char (chr, ord)

main :: IO ()
main = readFile "HelloWorld.bf" >>= runBF. parseBF

type BFSource = [BFCommand]

data BFCommand = GoRight
               | GoLeft
               | Increment
               | Decrement
               | Print
               | Read
               | LoopL
               | LoopR
               | Comment Char
               deriving (Eq, Show)

parseBF :: String -> BFSource 
parseBF = map charToBF
  where
    charToBF x = case x of
      '>' -> GoRight
      '<' -> GoLeft
      '+' -> Increment
      '-' -> Decrement
      '.' -> Print
      ',' -> Read
      '[' -> LoopL
      ']' -> LoopR

data Tape a = Tape [a] a [a]
  deriving (Show)

emptyTape :: Tape Int
emptyTape = Tape zeros 0 zeros
  where zeros = repeat 0

moveRight :: Tape a -> Tape a
moveRight (Tape ls p (r:rs)) = Tape (p:ls) r rs

moveLeft :: Tape a -> Tape a
moveLeft (Tape (l:ls) p rs) = Tape ls l (p:rs)

runBF :: BFSource -> IO ()
runBF = run emptyTape . bfSourceToTape
  where bfSourceToTape (b:bs) = Tape [] b bs

run :: Tape Int -> Tape BFCommand -> IO ()
run dataTape source@(Tape _ GoRight _) =
  advance (moveRight dataTape) source

run dataTape source@(Tape _ GoLeft  _) =
  advance (moveLeft dataTape) source

run (Tape l p r) source@(Tape _ Increment  _) =
  advance (Tape l (p+1) r) source

run (Tape l p r) source@(Tape _ Decrement  _) =
  advance (Tape l (p-1) r) source

run dataTape@(Tape _ p _) source@(Tape _ Print  _) = do
  putChar (chr p)
  hFlush stdout
  advance dataTape source
run dataTape@(Tape l _ r) source@(Tape _ Read  _) = do
  p <- getChar
  advance (Tape l (ord p) r) source

run dataTape@(Tape _ p _) source@(Tape _ LoopL  _)
  | p == 0 = seekLoopR 0 dataTape source
  | otherwise = advance dataTape source

run dataTape@(Tape _ p _) source@(Tape _ LoopR  _)
  | p /= 0 = seekLoopL 0 dataTape source
  | otherwise = advance dataTape source

run dataTape source@(Tape _ (Comment _) _) = advance dataTape source

seekLoopR :: Int -> Tape Int -> Tape BFCommand -> IO ()
seekLoopR 1 dataTape source@(Tape _ LoopR _) = advance dataTape source
seekLoopR b dataTape source@(Tape _ LoopR _) =
  seekLoopR (b-1) dataTape (moveRight source)
seekLoopR b dataTape source@(Tape _ LoopL _) =
  seekLoopR (b+1) dataTape (moveRight source)
seekLoopR b dataTape source =
  seekLoopR b dataTape (moveRight source)

seekLoopL :: Int -> Tape Int -> Tape BFCommand -> IO ()
seekLoopL 1 dataTape source@(Tape _ LoopL _) = advance dataTape source
seekLoopL b dataTape source@(Tape _ LoopL _) =
  seekLoopL (b-1) dataTape (moveLeft source)
seekLoopL b dataTape source@(Tape _ LoopR _) =
  seekLoopL (b+1) dataTape (moveLeft source)
seekLoopL b dataTape source =
  seekLoopL b dataTape (moveLeft source)

advance :: Tape Int -> Tape BFCommand -> IO ()
advance dataTape source = run dataTape (moveRight source)
advance dataTape (Tape _ _ []) = return ()
advance dataTape source = run dataTape (moveRight source)
