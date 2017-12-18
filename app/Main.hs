import Control.Concurrent
import Data.Sequence
import Control.Monad
import System.Random
import System.Environment

--Assumes redcode programs are txt files
--Found at ./redcode

main = do
  args <- getArgs
  --  let noProg = Prelude.length args
  -- ** I did attempt to load progs based on args
  -- This attempt is seen in genProgs
  -- But was unable to figure out how to carry IO Prog correctly in time

  --load in sample redcode programs
  redcodeProg <- readFile "./redcode/Dwarf.txt"
  let dwarf = redcodeParser (readProg redcodeProg)
  redcodeProg2 <- readFile "./redcode/Imp.txt"
  let imp = redcodeParser (readProg redcodeProg2)

  putStrLn $ "\nNumber of programs in this game: " ++ show noProg
  --Create core with redundant ADD instructions
  let baseCore = fromList $ map (baseCoreAddr) [0..coreSize-1]
  -- Randomly generate starting positions in Core
  -- As random can overwrite one another
  i <- randomRIO(0, coreSize-1)
  i2 <- randomRIO(0, coreSize-1)
  i3 <- randomRIO(0, coreSize-1)

  --Create MVar with starting indexs of Redcode programs
  index <- newMVar $ i    --Dwarf
  index2 <- newMVar $ i2  --Imp
  index3 <- newMVar $ i3 -- Dwarf2

  --insert the sample redcode programs into the core at the indexes specified
  let gameCore = insertRedcode (insertRedcode (insertRedcode baseCore i dwarf) i2 imp) i3 dwarf
  putStrLn $ "\nStart of game core: " ++ showSeq gameCore

  --Create MVar with game core
  coreMvar <- newMVar $ gameCore

  --Create stats MVar which will display current Stats on each turn
  -- [Number remaining/alive, number total]
  gameStats <- newMVar $ [noProg, noProg]

  runProgram 1 coreMvar index gameStats
  runProgram 2 coreMvar index2 gameStats
  runProgram 3 coreMvar index3 gameStats

  --delay the main thread to stop program from halting
  threadDelay (10^10)
  putStrLn "Game is a draw"
  threadDelay(10^8)  --Delay again so result seen
--end main

--Defined coreSize constant
coreSize = 40
noProg = 3 ::Int
--Generates Prog objects for all the prog names given
--Return IO Prog
genProgs :: [String] -> [IO Prog]
genProgs ([]) = []
genProgs (prog:progs) =
  readFiles prog : genProgs progs

--Read file and return the parsed Prog
readFiles :: String -> IO Prog
readFiles progName = do
  file <- (readFile $ "./redcode/" ++ progName ++ ".txt")
  let prog = redcodeParser $ readProg file
  return prog

--Takes String of Redcode program
--Returns list of lines in the redcode program as a list of individual words in line
readProg :: String -> [[String]]
readProg redcodeProg =
  let
    progIn = lines redcodeProg
    input = Prelude.filter isCommentLine progIn
    inputFiltered = map (Prelude.filter (/= ',')) input
    moves =  map words inputFiltered
    in (moves)

--inserts the Redcode program into the core at index specified
insertRedcode :: Seq CoreAddr ->Int -> Prog -> Seq CoreAddr
insertRedcode core index (Prog []) = core
insertRedcode core index (Prog (move:moves)) =
  insertRedcode (update index move core) ((index+1) `mod` coreSize) (Prog moves)

--Run the redcode programs on their own thread
runProgram pNo coreMvar indexMVar gameStats= do
  forkIO $ do
    runRedcode pNo coreMvar indexMVar gameStats


--Runs the redcode program with program number (pNo) in the core stored in coreMvar
--Starts at index stored at indexMVar
runRedcode :: Int -> MVar (Seq CoreAddr) -> MVar Int -> MVar [Int] -> IO ()
runRedcode pNo coreMvar indexMvar gameStats= do
    forever $ do
    --attempt to secure Core
      core <- takeMVar coreMvar
      curI <- takeMVar indexMvar
      stats <- takeMVar gameStats
      case (head stats) of
        1 -> announceWinner pNo
        _ -> do
          putStrLn $ "\n\nTotal number of prog: "++ show (last stats) ++ "\t\tNumber alive: " ++ show (head stats)
      putStrLn $ "\nCurrent core state: " ++ (showSeq core) ++ "\n"
      --Get current instruction
      let curCoreAddr = index core (curI `mod` coreSize)
      putStrLn $ "P" ++ show pNo ++ " excuting instruction at i (" ++ show curI ++ ")    "++ show curCoreAddr ++ "\n"
      case curCoreAddr of
        (CoreAddr DAT _ _) -> do
          -- Release core and set indexMVar as -1
          -- update gameStats to show prog terminated
          let newGameStats = ((head stats) - 1):(tail stats)
          putMVar indexMvar (-1)
          putMVar coreMvar core
          putMVar gameStats newGameStats
          terminateProg pNo
        (CoreAddr SPL a _) -> do
          --create another program at address in field A
          --With the next unused programNo
          let aIndex = applyAddrMode a curI core
          let noProg = (last stats)
          forkIO $ do
            newIndex <- newMVar $ aIndex
            runRedcode (noProg+1) coreMvar newIndex gameStats
          --continue current program at next instruction. Release mVars and sleep
          let newGameStats = ((head stats) + 1):[(noProg+1)]
          putMVar indexMvar (curI+1)
          putMVar coreMvar core
          putMVar gameStats newGameStats
          threadDelay(10^6)
        (CoreAddr _ _ _) -> do
          --excuted instruction and update both index and core
          let updatedCore = executeInstr core curCoreAddr curI
          let updatedIndex = (getNewIndex curCoreAddr curI) `mod` coreSize
          --release core and sleep to avoid starvation
          putMVar coreMvar updatedCore
          putMVar indexMvar updatedIndex
          putMVar gameStats stats
          threadDelay(10^6)

applyAddrMode :: Field -> Int -> Seq CoreAddr ->Int
applyAddrMode (Field Immed n) curI core = (curI + n) `mod` coreSize
applyAddrMode (Field Indirect n) curI core =
  let
    instr = index core (curI+n)
    instrVal = extractBVal instr
    in (curI + instrVal) `mod` coreSize
applyAddrMode (Field Direct n) curI core = (curI + n) `mod` coreSize
applyAddrMode (Field AutoDec n) curI core =
  let
    instr = index core (curI+n)
    instrVal = (extractBVal instr) -1
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
getNewIndex (CoreAddr DJN (Field addMode1 aVal) (Field addMode2 bVal)) curI =
    case (bVal-1) of
      0 -> (curI + 1)
      _ -> (curI + aVal)
--Default just to increment index/ got to next instruction
getNewIndex _ curI =
  curI + 1

--This is called once a DAT is execute, prints message and terminates thread
terminateProg pNo =
  do
      putStrLn $ "DAT executed!!  P" ++ show pNo ++ " has been terminated"
      tId <- myThreadId
      killThread tId

announceWinner pNo =
  do
    putStrLn $ "P" ++ show pNo ++ " has won the game!"
    tId <- myThreadId
    killThread tId

--Executes the Instruction given relative to curIndex given
--Applies result of this execution to the core
--Returns updated core with instruction applied
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
-- (JMP JMZ JMN DJN CMP)
executeInstr core _ _ = core


extractAVal :: CoreAddr -> Int
extractAVal (CoreAddr instr (Field _ val) b) = val
extractAVal (CoreAddr instr (Empty) b) = 0
extractBVal :: CoreAddr -> Int
extractBVal (CoreAddr instr a (Field _ val)) = val
extractBVal (CoreAddr instr a (Empty)) = 0

--Takes a CoreAddr and applies the Instr given to it's a and b field
--Returns the resulting CoreAddr
-- n1, n2 represent the numbers (Int) to be added/subbed to the a and b field respectively
updateCoreVal :: Instr -> CoreAddr -> Int -> Int -> CoreAddr
updateCoreVal ADD (CoreAddr instr (Field aAdrMode aVal) (Field bAdrMode bVal)) n1 n2 =
  CoreAddr instr (Field aAdrMode (aVal + n1)) (Field bAdrMode (bVal + n2))
updateCoreVal SUB (CoreAddr instr (Field aAdrMode aVal) (Field bAdrMode bVal)) n1 n2 =
  CoreAddr instr (Field aAdrMode (aVal - n1)) (Field bAdrMode (bVal - n2))
updateCoreVal ADD (CoreAddr instr (Empty) (Field bAdrMode bVal)) n1 n2 =
  CoreAddr instr (Empty) (Field bAdrMode (bVal + n2))
updateCoreVal SUB (CoreAddr instr (Empty) (Field bAdrMode bVal)) n1 n2 =
  CoreAddr instr (Empty) (Field bAdrMode (bVal - n2))

-- * *Parsing functions and types **

--Function for filtering commented lines in input
isCommentLine:: String -> Bool
isCommentLine "" = False
isCommentLine s
  | head s == ';' = False
  | otherwise = True

--Redcode prog parser
--Wrapper for Prog type constructor
redcodeParser :: [[String]] -> Prog
redcodeParser moves =
  Prog $ movesParser moves

--Takes string repesentation of instructions
--returns corresponding list of CoreAddr type
movesParser :: [[String]] -> [CoreAddr]
movesParser [] = []
movesParser (move:(moves)) =
  (moveParser move) : movesParser moves

--Takes string repesentation of an instruction
--returns corresponding CoreAddr type
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

--Takes a string representing an instructions, Parses the field A part
--Returns a Field type
fieldAParser :: [String] -> Field
fieldAParser move =
  Field addrMode $ read fieldVal
  where  f= move!!1
         addrMode = addrModeParser $ head f
         fieldVal = fieldValParser addrMode f

--Takes a string representing an instructions, Parses the field B part
--Returns a Field type
fieldBParser :: [String] -> Field
fieldBParser move=
  Field addrMode $ read fieldVal
  where  f= last move
         addrMode = addrModeParser $ head f
         fieldVal = fieldValParser addrMode f

--Separates chars representing Addressing modes from value
fieldValParser :: AddrMode -> String -> String
fieldValParser Direct field
  | head field == '@' = tail field
  | head field == '$' = tail field
  | head field == '#' = tail field
  | head field == '<' = tail field
  | otherwise = field
fieldValParser _ field = tail field

--Converts char repesenting Addressing Mode to a AddrMode type
--Defaults to Direct
addrModeParser :: Char -> AddrMode
addrModeParser '<' = AutoDec
addrModeParser '@' = Indirect
addrModeParser '#' = Immed
addrModeParser '$' = Direct
addrModeParser _ = Direct --Can not specify any addmode if Direct. this could be a sign or a num

--Wrapper for showSeq
--Starts showSeq2 at index 0
showSeq :: Seq CoreAddr -> String
showSeq core = showSeq2 core 0

--Builds string representing a Seq CoreAddr
--Each CoreAddr is printed on a new line
--This is to be able to print a readable string of the Core
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

--Provides Basic coreAddr which essentially has no effect
--Used instead of MOV to allow user to see Imp progress easier
baseCoreAddr:: Int -> CoreAddr
baseCoreAddr n = CoreAddr ADD (Field Immed 0) (Field Direct 0)

--redcode data types
--First int is the progNo
--Second Int is the curIndex in MARS
data Prog = Prog [CoreAddr]
  deriving (Show, Read)
