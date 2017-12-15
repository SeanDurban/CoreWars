{-# LANGUAGE OverloadedStrings #-}
import Control.Concurrent
import Data.Sequence
import Control.Monad
import Ansi

main = do
--  let dwarf = redcodeParser (readProg "./redcodeProg/Dwarf.txt") 1
--  let imp = redcodeParser (readProg "./redcodeProg/Imp.txt") 2
  redcodeProg <- readFile "./redcodeProg/Dwarf.txt"
  let dwarf = redcodeParser (readProg redcodeProg) 0
  redcodeProg2 <- readFile "./redcodeProg/Imp.txt"
  let imp = redcodeParser (readProg redcodeProg2) 1
  redcodeProg3 <- readFile "./redcodeProg/Gemini.txt"
  let gemini = redcodeParser (readProg redcodeProg3) 2
--  let dwarf =readT2 "./redcodeProg/Dwarf.txt"

  let baseCore = fromList $ map (getCoreAddr) [0..coreSize-1]

  let gameCore = ( insertRedcode baseCore 0 gemini)
  putStrLn $ "game core: " ++ showSeq gameCore

  m <- newMVar $ gameCore

  index <- newMVar $ 2
  index2 <- newMVar $ 13

  runRedcode 2 m index
--  runRedcode 3 m index2

  threadDelay (10^8)
--end main


readT2 fileName = do
  redcodeProg <- readFile fileName
  return (redcodeParser (readProg redcodeProg) 0)

readRedcodeFile fileName =
    readFile fileName >>= \redcodeProg ->
    return (readProg redcodeProg)
--readProg :: String -> [[String]]
readProg redcodeProg =
  let
  --redcodeProg <- readFile fileName
    progIn = lines redcodeProg
    input = Prelude.filter stripComments progIn
    inputFiltered = map (Prelude.filter (/= ',')) input
    moves =  map words inputFiltered
    in (moves)


coreSize = 25


--inserts the Redcode program into the core at index specified
insertRedcode :: Seq CoreAddr ->Int -> Prog -> Seq CoreAddr
insertRedcode core index (Prog _ _ []) = core
insertRedcode core index (Prog pNo curI (move:moves)) =
  insertRedcode (update index move core) ((index+1) `mod` coreSize) (Prog pNo curI moves)

runRedcode pNo coreMvar indexMvar= do
  forkIO $ do
    forever $ do
    --attempt to secure Core
      core <- takeMVar coreMvar
      curI <- takeMVar indexMvar
    --  goto (xStart+curI) 20 >> color c "_"
      --putStrLn $ "i: " ++ show curI
    --  printMove (curI `mod` coreSize) color
  --    putStrLn $ "Prog"++show pNo ++ " acquired core\n"
      putStrLn $ "\n\nCurrent core state: " ++ (showSeq core) ++ "\n"
      let curCoreAddr = index core (curI `mod` coreSize)
      putStrLn $ show pNo ++ " excuting instruction at i (" ++ show curI ++ ")    "++ show curCoreAddr ++ "\n"
  --    checkDAT curCoreAddr
      case curCoreAddr of
        (CoreAddr DAT _ _) -> do
          -- Release core and set indexMVar as -1
          putMVar indexMvar (-1)
          putMVar coreMvar core
          terminateProg pNo
        (CoreAddr _ _ _) -> do
          let updatedCore = executeInstr core curCoreAddr curI
          let updatedIndex = (getNewIndex curCoreAddr curI) `mod` coreSize
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
getNewIndex (CoreAddr JMP (Field addrMode aVal) Empty) curI =  curI + aVal
getNewIndex (CoreAddr JMZ (Field addMode1 aVal) (Field addMode2 bVal)) curI =
    case bVal of
      0 -> (curI + aVal)
      _ -> (curI)
getNewIndex (CoreAddr JMN (Field addMode1 aVal) (Field addMode2 bVal)) curI =
    case bVal of
      0 -> (curI)
      _ -> (curI + aVal)
getNewIndex (CoreAddr CMP (Field addMode1 aVal) (Field addMode2 bVal)) curI =
    case (aVal - bVal) of
      0 -> (curI + 2)
      _ -> (curI + 1)
getNewIndex _ curI =
  curI + 1
--need DJN CMP


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
    newBCoreAddr = CoreAddr DAT Empty (Field Immed aVal)
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
  CoreAddr instr (Field aAdrMode (aVal + n1)) (Field bAdrMode (bVal + n2))
updateCoreVal SUB (CoreAddr instr (Field aAdrMode aVal) (Field bAdrMode bVal)) n1 n2 =
  CoreAddr instr (Field aAdrMode (aVal - n1)) (Field bAdrMode (bVal - n2))
updateCoreVal ADD (CoreAddr instr (Empty) (Field bAdrMode bVal)) n1 n2 =
  CoreAddr instr (Empty) (Field bAdrMode (bVal + n2))
updateCoreVal SUB (CoreAddr instr (Empty) (Field bAdrMode bVal)) n1 n2 =
  CoreAddr instr (Empty) (Field bAdrMode (bVal - n2))


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

showSeq :: Seq CoreAddr -> String
showSeq core = showSeq2 core 0

showSeq2 :: Seq CoreAddr -> Int -> String
showSeq2 core i
  | i == Data.Sequence.length core = "\n"
  | otherwise = "\n" ++ show i ++ ": " ++ show (index core i) ++ (showSeq2 core $ i+1)
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
