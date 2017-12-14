{-# LANGUAGE OverloadedStrings #-}
import Control.Concurrent
import Data.Sequence
import Control.Monad

main = do
  redcodeProg <- readFile "./redcodeProg/Dwarf.txt"
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
  index2 <- newMVar $ 5
  index3 <- newMVar $ 10
  runRedcode 1 m index
  runRedcode 2 m index2
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
      let curCoreAddr = index core (curI `mod` 15)
      putStrLn $ " Instruction at i " ++ show curCoreAddr ++ "\n"
      let updatedCore = executeInstr core curCoreAddr curI
      let updatedIndex = getNewIndex curCoreAddr curI
      putMVar coreMvar updatedCore
      putMVar indexMvar updatedIndex
      putStrLn $ "Prog"++show pNo ++ " released core, new index:  " ++ show updatedIndex ++ "\n"
      --release core and sleep to avoid starvation
      threadDelay(10^7) --could make this random

test coreMvar core =
  putMVar coreMvar core

applyAddrMode :: Field -> Int -> Seq CoreAddr ->Int
applyAddrMode (Field Immed n) curI core = curI + n
applyAddrMode (Field Indirect n) curI core =  --want to go get value of that addr
  let
    instr = index core (curI+n)  --get coreInstr at curI +n
    instrVal = 0 --val = extractVal instr
    in curI + instrVal
applyAddrMode (Field Direct n) curI core = curI + n
applyAddrMode (Field AutoDec n) curI core = --n and treated as indirect then decr value at n
  let
    instr = index core (curI+n)  --get coreInstr at curI +n
    instrVal = 0 --val = extractVal instr -1
    in curI + instrVal

getNewIndex :: CoreAddr -> Int -> Int
getNewIndex (CoreAddr JMP (Field addrMode aVal) Empty) curI =  (curI + aVal) `mod` 15
getNewIndex (CoreAddr JMZ (Field addMode1 aVal) (Field addMode2 bVal)) curI =
    case bVal of
      0 -> (curI + aVal) `mod` 15
      _ -> (curI) `mod` 15
getNewIndex (CoreAddr JMN (Field addMode1 aVal) (Field addMode2 bVal)) curI =
    case bVal of
      0 -> (curI) `mod` 15
      _ -> (curI + aVal) `mod` 15
getNewIndex (CoreAddr CMP (Field addMode1 aVal) (Field addMode2 bVal)) curI =
    case (aVal - bVal) of
      0 -> (curI+2) `mod` 15
      _ -> (curI + 1) `mod` 15
getNewIndex _ curI =
  curI + 1
--need DJN CMP

-- CoreAddr Instr Field Field
--Field AddrMode Int
executeInstr :: Seq CoreAddr -> CoreAddr -> Int -> Seq CoreAddr
executeInstr core (CoreAddr MOV (Field Immed aVal) b) curIndex =
  let
    bIndex = applyAddrMode b curIndex core
    newBCoreAddr = CoreAddr DAT (Field Immed aVal) Empty
    in (update bIndex newBCoreAddr core)
executeInstr core (CoreAddr MOV a b) curIndex =
  let
    aIndex = applyAddrMode a curIndex core
    bIndex = applyAddrMode b curIndex core
    aCoreAddr  = index core aIndex
    in (update bIndex aCoreAddr core)
executeInstr core (CoreAddr ADD (Field Immed aVal) b) curIndex =
  let
    bIndex = applyAddrMode b curIndex core
    bCoreAddr  = index core bIndex
    newBCoreAddr = updateCoreVal ADD bCoreAddr 0 aVal
    in (update bIndex newBCoreAddr core)

executeInstr core (CoreAddr ADD a b) curIndex =
  let
    aIndex = applyAddrMode a curIndex core
    bIndex = applyAddrMode b curIndex core
    bCoreAddr  = index core bIndex
    aCoreAddr  = index core aIndex
    n1 = extractAVal aCoreAddr
    n2 = extractBVal bCoreAddr
    newBCoreAddr = updateCoreVal ADD bCoreAddr n1 n2
    in (update bIndex newBCoreAddr core)

executeInstr core (CoreAddr SUB (Field Immed aVal) b) curIndex =
  let
    bIndex = applyAddrMode b curIndex core
    bCoreAddr  = index core bIndex
    newBCoreAddr = updateCoreVal SUB bCoreAddr 0 aVal
    in (update bIndex newBCoreAddr core)

executeInstr core (CoreAddr SUB a b) curIndex =
  let
    aIndex = applyAddrMode a curIndex core
    bIndex = applyAddrMode b curIndex core
    bCoreAddr  = index core bIndex
    aCoreAddr  = index core aIndex
    n1 = extractAVal aCoreAddr
    n2 = extractBVal bCoreAddr
    newBCoreAddr = updateCoreVal SUB bCoreAddr n1 n2
    in (update bIndex newBCoreAddr core)

--All other instr don't change the core
-- JMP JMZ JMN DJN CMP
executeInstr core _ _ = core


extractAVal :: CoreAddr -> Int
extractAVal (CoreAddr instr (Field _ val) b) = val
extractAVal (CoreAddr instr (Empty) b) = 0

extractBVal :: CoreAddr -> Int
extractBVal (CoreAddr instr a (Field _ val)) = val
extractBVal (CoreAddr instr a (Empty)) = 0


--Will need to add cases here to deal with Empty Fields
updateCoreVal :: Instr -> CoreAddr -> Int -> Int -> CoreAddr
--updateCoreVal ADD (CoreAddr instr a (Field adrMode bVal)) _ n2 =
--  CoreAddr instr a (Field adrMode ((bVal + n2) `mod` 15))
updateCoreVal ADD (CoreAddr instr (Field aAdrMode aVal) (Field bAdrMode bVal)) n1 n2 =
  CoreAddr instr (Field aAdrMode ((aVal + n1) `mod` 15)) (Field bAdrMode ((bVal + n2) `mod` 15))
updateCoreVal SUB (CoreAddr instr (Field aAdrMode aVal) (Field bAdrMode bVal)) n1 n2 =
  CoreAddr instr (Field aAdrMode ((aVal - n1) `mod` 15)) (Field bAdrMode ((bVal - n2) `mod` 15))


extractAField :: CoreAddr -> Int
--extractAField _ (Field Direct n) _ = n
extractAField _ = -1

extractInstr :: CoreAddr -> Instr
--extractInstr MOV a b = MOV
extractInstr _ = DAT

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
  deriving (Show, Read, Eq)

data Field = Field AddrMode Int | Empty
  deriving (Show, Read)

data AddrMode = Immed | Indirect | Direct | AutoDec
  deriving (Show, Read, Eq)

getCoreAddr:: Int -> CoreAddr
getCoreAddr n = CoreAddr ADD (Field Direct 0) (Field Direct 1)

--redcode data types
--First in is the progNo
--Second Int is the curIndex in MARS
data Prog = Prog Int Int [Move]
  deriving (Show, Read)
data Move = Move Instr Field Field
  deriving (Show, Read)
