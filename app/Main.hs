{-# LANGUAGE OverloadedStrings #-}
import Control.Concurrent
import Data.Sequence

main = do
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

data Core = Core (Seq CoreAddr)
  deriving (Show,Read)

data CoreAddr = CoreAddr Instr Field Field
  deriving (Show, Read)

data Instr = DAT | MOV | ADD
  deriving (Show, Read)

data Field = AddrMode Int | Empty
  deriving (Show, Read)

data AddrMode = Immed| Indiret -- | # | < | Empty
  deriving (Show, Read)

getCoreAddr:: Int -> CoreAddr
getCoreAddr n = CoreAddr DAT Empty $ AddrMode n
