{-# LANGUAGE OverloadedStrings #-}
import Control.Concurrent
import Data.Sequence
import Control.Monad
import Ansi

main = do
  redcodeProg <- readFile "./redcodeProg/Dwarf.txt"
  let progIn = lines redcodeProg
  let input = Prelude.filter stripComments progIn
  let inputFiltered = map (Prelude.filter (/= ',')) input
  let moves =  map words inputFiltered
  let dwarf = redcodeParser moves 1
  putStrLn $ show dwarf
  let core = fromList $ map (getCoreAddr) [0..coreSize]
  --let core2 = update 1 (CoreAddr MOV (Field Direct 0) (Field Direct 1) ) core
  let core2 = update 0 (CoreAddr ADD (Field Immed 5) (Field Direct (3)) ) core
  let core3 = update 1 (CoreAddr MOV (Field Immed 0) (Field Indirect (2))) core2
  let core4 = update 2 (CoreAddr JMP (Field Direct (-2)) Empty) core3
  let core5 = update 3 (CoreAddr DAT Empty (Field Direct (-1))) core4
  let core6 = update 4 (CoreAddr MOV (Field Direct 0) (Field Direct 1)) core5
  m <- newMVar $ core5

  index <- newMVar $ 0
  index2 <- newMVar $ 4
  index3 <- newMVar $ 10
  runRedcode 1 m index Red
  --runRedcode 2 m index2
  --runRedcode 3 m index3

  threadDelay (10^8)
--end main



coreSize = 9


runRedcode pNo coreMvar indexMvar c= do
  forkIO $ do
    forever $ do
    --attempt to secure Core
      core <- takeMVar coreMvar
      curI <- takeMVar indexMvar
      goto (xStart+curI) 20 >> color c "_"
      putStrLn $ "i: " ++ show curI
    --  printMove (curI `mod` coreSize) color
  --    putStrLn $ "Prog"++show pNo ++ " acquired core\n"
  ---    putStrLn $ "\n\nCurrent core state: " ++ show core ++ "\n\n"
      let curCoreAddr = index core (curI `mod` coreSize)
  --    putStrLn $ "Instruction at i " ++ show curI ++ "    "++ show curCoreAddr ++ "\n"
  --    checkDAT curCoreAddr
      case curCoreAddr of
        (CoreAddr DAT _ _) -> do
          -- Release core and set indexMVar as -1
          putMVar indexMvar (-1)
          putMVar coreMvar core
          terminateProg pNo
        (CoreAddr _ _ _) -> do
          let updatedCore = executeInstr core curCoreAddr curI
          let updatedIndex = getNewIndex curCoreAddr curI
          putMVar coreMvar updatedCore
          putMVar indexMvar updatedIndex
  --        putStrLn $ "Prog"++show pNo ++ " released core, new index:  " ++ show updatedIndex ++ "\n"
          --release core and sleep to avoid starvation
          threadDelay(10^6) --could make this random

printMove :: Int -> Colour -> IO()
printMove n c =   goto (xStart+n) 20 >> color c "_"


xStart = 10

applyAddrMode :: Field -> Int -> Seq CoreAddr ->Int
applyAddrMode (Field Immed n) curI core = (curI + n) `mod` coreSize
applyAddrMode (Field Indirect n) curI core =  --want to go get value of that addr
  let
    instr = index core (curI+n)  --get coreInstr at curI +n
    instrVal = extractBVal instr
    in (curI + instrVal) `mod` coreSize
applyAddrMode (Field Direct n) curI core = (curI + n) `mod` coreSize
applyAddrMode (Field AutoDec n) curI core = --n and treated as indirect then decr value at n
  let
    instr = index core (curI+n)  --get coreInstr at curI +n
    instrVal = (extractBVal instr) -1--val = extractVal instr -1
    in (curI + instrVal) `mod` coreSize

getNewIndex :: CoreAddr -> Int -> Int
getNewIndex (CoreAddr JMP (Field addrMode aVal) Empty) curI =  (curI + aVal) `mod` coreSize
getNewIndex (CoreAddr JMZ (Field addMode1 aVal) (Field addMode2 bVal)) curI =
    case bVal of
      0 -> (curI + aVal) `mod` coreSize
      _ -> (curI) `mod` coreSize
getNewIndex (CoreAddr JMN (Field addMode1 aVal) (Field addMode2 bVal)) curI =
    case bVal of
      0 -> (curI) `mod` coreSize
      _ -> (curI + aVal) `mod` coreSize
getNewIndex (CoreAddr CMP (Field addMode1 aVal) (Field addMode2 bVal)) curI =
    case (aVal - bVal) of
      0 -> (curI+2) `mod` coreSize
      _ -> (curI + 1) `mod` coreSize
getNewIndex _ curI =
  curI + 1
--need DJN CMP

checkDAT (CoreAddr DAT a b) =
  do
    forever $ do
      putStrLn "DAT executed!!"
      threadDelay(10^8)
checkDAT (_) = putStrLn $ "\n"

terminateProg pNo =
  do
    forever $ do
      putStrLn $ "DAT executed!!  P" ++ show pNo ++ " has been terminated"
      threadDelay(10^8)
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
--  CoreAddr instr a (Field adrMode ((bVal + n2) `mod` coreSize))
updateCoreVal ADD (CoreAddr instr (Field aAdrMode aVal) (Field bAdrMode bVal)) n1 n2 =
  CoreAddr instr (Field aAdrMode ((aVal + n1) `mod` coreSize)) (Field bAdrMode ((bVal + n2) `mod` coreSize))
updateCoreVal SUB (CoreAddr instr (Field aAdrMode aVal) (Field bAdrMode bVal)) n1 n2 =
  CoreAddr instr (Field aAdrMode ((aVal - n1) `mod` coreSize)) (Field bAdrMode ((bVal - n2) `mod` coreSize))
updateCoreVal ADD (CoreAddr instr (Empty) (Field bAdrMode bVal)) n1 n2 =
  CoreAddr instr (Empty) (Field bAdrMode ((bVal + n2) `mod` coreSize))
updateCoreVal SUB (CoreAddr instr (Empty) (Field bAdrMode bVal)) n1 n2 =
  CoreAddr instr (Empty) (Field bAdrMode ((bVal - n2) `mod` coreSize))


extractAField :: CoreAddr -> Int
--extractAField _ (Field Direct n) _ = n
extractAField _ = -1

extractInstr :: CoreAddr -> Instr
--extractInstr MOV a b = MOV
extractInstr _ = DAT

sampleCoreAddr = CoreAddr DAT Empty $ Field Indirect 3

sampleImp =Prog 1 0 [CoreAddr MOV (Field Direct 0) (Field Direct 1)]

stripComments:: String -> Bool
stripComments "" = False
stripComments s
  | head s == ';' = False
  | otherwise = True

--Redcode prog parser
redcodeParser :: [[String]] -> Int -> Prog
redcodeParser moves progNo =
  Prog progNo 0 $ movesParser moves

movesParser :: [[String]] -> [CoreAddr]
movesParser [] = []
movesParser (move:(moves)) =
  (moveParser move) : movesParser moves


moveParser :: [String] -> CoreAddr
moveParser move
  | instr == "DAT" = CoreAddr DAT Empty fieldA
  | instr == "ADD" = CoreAddr ADD fieldA fieldB
  | instr == "SUB" = CoreAddr SUB fieldA fieldB
  | instr == "MOV" = CoreAddr MOV fieldA fieldB
  | instr == "JMP" = CoreAddr JMP fieldA Empty
  | instr == "SPL" = CoreAddr SPL fieldA Empty
  | otherwise = CoreAddr DAT fieldA fieldB
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
getCoreAddr n = CoreAddr ADD (Field Immed 0) (Field Direct 0)

--redcode data types
--First in is the progNo
--Second Int is the curIndex in MARS
data Prog = Prog Int Int [CoreAddr]
  deriving (Show, Read)
