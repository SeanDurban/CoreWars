{-# LANGUAGE OverloadedStrings #-}
import Control.Concurrent
import Data.Sequence
import Control.Monad

main = do
  redcodeProg <- readFile "./redcodeProg/Imp.txt"
  let progIn = lines redcodeProg
  let input = Prelude.filter stripComments progIn
  let inputFiltered = map (Prelude.filter (/= ',')) input
  let moves =  map words inputFiltered
  putStrLn $ show moves
  let dwarf = redcodeParser moves 1
  putStrLn $ show dwarf
  let core = fromList $ map (getCoreAddr) [0..15]
  let core2 = update 1 (CoreAddr MOV (Field Direct 0) (Field Direct 1) ) core
  m <- newMVar $ core2

  index <- newMVar $ 1
  index2 <- newMVar $ 46
  index3 <- newMVar $ 38
  runRedcode 1 m index
  --runRedcode 2 m index2
  --runRedcode 3 m index3

  threadDelay (10^8)
  --putStrLn $ show core

--end main

communicate n m = do
  forkIO $ do
    seq1 <- takeMVar m
    putStrLn (show n ++ " received " ++ show seq1)
    putStrLn $ show n ++ " sending"
    putMVar m $ update 0 (getCoreAddr n) seq1


--runRedcode (Prog pNo i moves:[]) = runProg pNo i

--runRedcode :: Prog -> MVar
--runRedcode pNo i moves coreMvar= do
runRedcode pNo coreMvar indexMvar= do
  forkIO $ do
    forever $ do
    --attempt to secure Core
      core <- takeMVar coreMvar
      curI <- takeMVar indexMvar
      putStrLn $ "Prog"++show pNo ++ " acquired core\n"
    --  instruction <- readInstruction i pNo core
    --  let i= execute instruction core
      let curInstr = index core curI
      putStrLn $ " Instruction at i " ++ show curInstr ++ "\n"
      let updatedCore = executeInstr core curI
      let index2 = (curI+1) `mod` 15
      putMVar coreMvar $ update 0 sampleCoreAddr core
      putMVar indexMvar index2
      putStrLn $ "Prog"++show pNo ++ " released core " ++ show index2 ++ "\n"
      --release core and sleep to avoid starvation
      threadDelay(10^7) --could make this random

executeInstr :: Seq CoreAddr -> Int -> Seq CoreAddr
executeInstr core curI
  | coreAddr == MOV = update fieldA (index core curI) core
  | otherwise = core
  where oldCoreAddr = index core curI
        coreAddr = extractInstr oldCoreAddr
        fieldA = extractAField oldCoreAddr
        fieldB = Empty

extractAField :: CoreAddr -> Int
extractAField _ (Field Direct n) _ = n
extractAField _ _ _ = -1

extractInstr :: CoreAddr -> Instr
extractInstr MOV a b = MOV
extractInstr _ _ _ = DAT

sampleCoreAddr = CoreAddr DAT Empty $ Field Indirect 3

sampleImp =Prog 1 0 [Move MOV (Field Direct 0) (Field Direct 1)]

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
  | instr == "DAT" = Move DAT Empty fieldA
  | instr == "ADD" = Move ADD fieldA fieldB
  | instr == "SUB" = Move SUB fieldA fieldB
  | instr == "MOV" = Move MOV fieldA fieldB
  | instr == "JMP" = Move JMP fieldA Empty
  | instr == "SPL" = Move SPL fieldA Empty
  | otherwise = Move DAT fieldA fieldB
  where
    instr = head move
    fieldA = fieldAParser move
    fieldB = fieldBParser move

fieldAParser :: [String] -> Field
fieldAParser move =
  Field addrMode $ read fieldVal
  where  f= move!!1
         addrMode = addrModeParser $ head f
         fieldVal = fieldValParser addrMode f

fieldBParser :: [String] -> Field
fieldBParser move=
  Field addrMode $ read fieldVal
  where  f= last move
         addrMode = addrModeParser $ head f
         fieldVal = fieldValParser addrMode f

fieldValParser :: AddrMode -> String -> String
fieldValParser Direct field
  | head field == '@' = tail field
  | otherwise = field
fieldValParser _ field = tail field

addrModeParser :: Char -> AddrMode
addrModeParser '<' = AutoDec
addrModeParser '@' = Indirect
addrModeParser '#' = Immed
addrModeParser '$' = Direct
addrModeParser _ = Direct --Can not specify any addmode if Direct. this could be a sign or a num


--MARS simulator data types
data Core = Core (Seq CoreAddr)
  deriving (Show,Read)

data CoreAddr = CoreAddr Instr Field Field
  deriving (Show, Read)

data Instr = DAT | MOV | ADD | SUB | JMP | JMZ | JMN | DJN | CMP | SPL
  deriving (Show, Read)

data Field = Field AddrMode Int | Empty
  deriving (Show, Read)

data AddrMode = Immed | Indirect | Direct | AutoDec -- | # | < | Empty
  deriving (Show, Read)

getCoreAddr:: Int -> CoreAddr
getCoreAddr n = CoreAddr ADD (Field Direct 0) (Field Direct 1)

--redcode data types
--First in is the progNo
--Second Int is the curIndex in MARS
data Prog = Prog Int Int [Move]
  deriving (Show, Read)
data Move = Move Instr Field Field
  deriving (Show, Read)
