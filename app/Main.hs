{-# LANGUAGE OverloadedStrings #-}
import Control.Concurrent
import Data.Sequence

main = do
  redcodeProg <- readFile "./redcodeProg/Imp.txt"
  let x = lines redcodeProg
  putStrLn $ show x
  let core = fromList [getCoreAddr 1, getCoreAddr 2]
  m <- newMVar $ core
  communicate 1 m
  communicate 2 m
  communicate 3 m
  communicate 4 m
  communicate 0 m
  threadDelay (10^5)
  --forkIO (do putMVar m 'a'; putMVar m 'b')
  --c <- takeMVar m
  --print c
  --c <- takeMVar m
  --print c
--communicate :: Int -> MVar
communicate n m = do
--  m <- newEmptyMVar
  forkIO $ do
    seq1 <- takeMVar m
    putStrLn (show n ++ " received " ++ show seq1)
    putStrLn $ show n ++ " sending"
    putMVar m $ update 0 (getCoreAddr n) seq1
--MARS simulator data types
data Core = Core (Seq CoreAddr)
  deriving (Show,Read)

data CoreAddr = CoreAddr Instr Field Field
  deriving (Show, Read)

data Instr = DAT | MOV | ADD | SUB | JMP
  deriving (Show, Read)

data Field = Field AddrMode Int | Empty
  deriving (Show, Read)

data AddrMode = Immed | Indirect  -- | # | < | Empty
  deriving (Show, Read)

getCoreAddr:: Int -> CoreAddr
getCoreAddr n = CoreAddr DAT Empty $ Field Indirect n

--redcode data types

data Prog = Prog Int [Move]
  deriving (Show, Read)
data Move = Move Instr Field Field
  deriving (Show, Read)
