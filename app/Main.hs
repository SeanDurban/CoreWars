{-# LANGUAGE OverloadedStrings #-}
import Control.Concurrent
import Data.Sequence

main = do
  redcodeProg <- readFile "./redcodeProg/Dwarf.txt"
  let progIn = lines redcodeProg
  let input = Prelude.filter stripComments progIn
  let inputFiltered = map (Prelude.filter (/= ',')) input
  let moves =  map words inputFiltered
  putStrLn $ show moves
  let core = fromList [getCoreAddr 1, getCoreAddr 2]
  m <- newMVar $ core
--  communicate 1 m
  --communicate 2 m
  --communicate 3 m
  communicate 4 m
  communicate 0 m
  threadDelay (10^5)
--end main

communicate n m = do
  forkIO $ do
    seq1 <- takeMVar m
    putStrLn (show n ++ " received " ++ show seq1)
    putStrLn $ show n ++ " sending"
    putMVar m $ update 0 (getCoreAddr n) seq1

stripComments:: String -> Bool
stripComments "" = False
stripComments s
  | head s == ';' = False
  | otherwise = True

--Redcode prog parser
redcodeParser :: [[String]] -> Int -> Prog
redcodeParser moves progNo =
  Prog progNo 0 $ movesParser moves

movesParser :: [[String]] -> [Move]
movesParser [] = []
movesParser (move:(moves)) =
  (moveParser move) : movesParser moves


moveParser :: [String] -> Move
moveParser move
  | instr == "DAT" = Move DAT fieldA fieldB
  | instr == "ADD" = Move ADD fieldA fieldB
  where
    instr = head move
    fieldA = fieldAParser ""
    fieldB = fieldBParser ""

fieldAParser :: String -> Field
fieldAParser _ = Empty
fieldBParser :: String -> Field
fieldBParser _ = Empty

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
--First in is the progNo
--Second Int is the curIndex in MARS
data Prog = Prog Int Int [Move]
  deriving (Show, Read)
data Move = Move Instr Field Field
  deriving (Show, Read)
