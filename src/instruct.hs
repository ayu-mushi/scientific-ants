-- 参考: http://coderepos.org/share/browser/lang/csharp/Tierra/trunk 

module ScientificAnts.InstructionSet where

import Control.Lens
import Control.Arrow
import Control.Monad
import Control.Applicative
import Data.Array
import Data.Bits
import Data.List
import System.Random
import Data.Maybe

import ScientificAnts.Simulation

getTheAnt :: (Ant -> Instruction) -> Instruction
getTheAnt f i world = f ((world ^. ants) ! i) i world

modifyTheAnt's x f i = (ants <<< (ix i) <<< x) %~ f

setTheAnt's x a i = (ants <<< (ix i) <<< x) .~ a

shl :: Instruction
shl = modifyTheAnt's (register <<< _3) $ flip shiftL 1

zero :: Instruction
zero = setTheAnt's (register <<< _3) 0

ifz :: Instruction
ifz i world = if cx == 0 then world else incIP i world
  where cx = ((world ^. ants) ! i) ^. (register <<< _3)

subCAB :: Instruction
subCAB i world = world & ((ants <<< (ix i) <<< register <<< _3) .~ (ax - bx))
  where
    ax = (reg ^. _1)
    bx = (reg ^. _2)
    reg = (world ^. ants) ! i ^. register

subAAC :: Instruction
subAAC i world = world & (ants <<< (ix i) <<< register <<< _1) -~ cx
  where
    cx = (reg ^. _1)
    reg = ((world ^. ants) ! i) ^. register

incA :: Instruction
incA = modifyTheAnt's (register <<< _1) (+ 1)

incB :: Instruction
incB = modifyTheAnt's (register <<< _2) (+ 1)

decC :: Instruction
decC = modifyTheAnt's (register <<< _3) $ flip (-) 1

incC :: Instruction
incC = modifyTheAnt's (register <<< _3) (+ 1)

-- 手続きの実行が失敗した時などに減点する
err:: Instruction
err = modifyTheAnt's hunger $ flip (-) 1

pushToTheStack :: Int -> Instruction
pushToTheStack x i world =
  if (length xs) >= 10
    then err i world
    else world & ((ants <<< (ix i) <<< stack) %~ ((:) x))
  where
    xs = ((world ^. ants) ! i) ^. stack

pushA :: Instruction
pushA i world =
  if (length xs) >= 10 
    then err i world
    else world & ((ants <<< (ix i) <<< stack) %~ ((:) ax))
  where
    ax = ((world ^. ants) ! i) ^. (register <<< _1)
    xs = ((world ^. ants) ! i) ^. stack

pushB :: Instruction
pushB i world =
  if (length xs) >= 10 
    then err i world
    else world & ((ants <<< (ix i) <<< stack) %~ ((:) bx))
  where
    bx = ((world ^. ants) ! i) ^. (register <<< _2)
    xs = ((world ^. ants) ! i) ^. stack

pushC :: Instruction
pushC i world =
  if (length xs) >= 10 
    then err i world
    else world & ((ants <<< (ix i) <<< stack) %~ ((:) cx))
  where
    cx = ((world ^. ants) ! i) ^. (register <<< _3)
    xs = ((world ^. ants) ! i) ^. stack

pushD :: Instruction
pushD i world =
  if (length xs) >= 10 
    then err i world
    else world & ((ants <<< (ix i) <<< stack) %~ ((:) cx))
  where
    cx = ((world ^. ants) ! i) ^. (register <<< _4)
    xs = ((world ^. ants) ! i) ^. stack

-- スタックの一番上の値を破棄する
discard :: Instruction
discard = modifyTheAnt's stack tail

popA :: Instruction
popA i world =
  if null $ ((world ^. ants) ! i) ^. stack then err i world
    else (discard i world) & (ants <<< (ix i) <<< register <<< _1) .~ x
  where
    x = head (((world ^. ants) ! i) ^. stack)

popB :: Instruction
popB i world =
  if null $ ((world ^. ants) ! i) ^. stack then err i world
    else (discard i world) & (ants <<< (ix i) <<< register <<< _2) .~ x
  where
    x = head (((world ^. ants) ! i) ^. stack)

popC :: Instruction
popC i world = 
  if null $ ((world ^. ants) ! i) ^. stack then err i world
    else (discard i world) & (ants <<< (ix i) <<< register <<< _3) .~ x
  where
    x = head (((world ^. ants) ! i) ^. stack)

popD :: Instruction
popD i world =
  if null $ ((world ^. ants) ! i) ^. stack then err i world
    else (discard i world) & (ants <<< (ix i) <<< register <<< _4) .~ x
  where
    x = head (((world ^. ants) ! i) ^. stack)

data SearchDirection = Forward | Backward | Outward

readPattern :: Genome -> Int -> [Int]
readPattern gs i =
  if ((size gs) <= i || i < 0)
    then []
  else if (gs ! i) == 0
    then 0 : readPattern gs (i + 1)
  else if (gs ! i) == 1
    then 1 : readPattern gs (i + 1)
  else
    []

reverseTranscriptase :: [Int] -> [Int]
reverseTranscriptase = map rt
  where
    rt 0 = 1
    rt 1 = 0
    rt _ = error "reverseTranscriptasing list must consist of only 0 or 1."

findForward :: Genome -> Int -> [Int] -> Maybe Int
findForward gs i pattern =
  if (size gs) <= i || i < 0 || null pattern
    then Nothing
  else if (take (length pattern) $ readPattern gs (i + 1)) == pattern
    then Just $ i + (length pattern) + 1
    else findForward gs (i + 1) pattern

findBackward :: Genome -> Int -> [Int] -> Maybe Int
findBackward gs i pattern = 
  if i < 0 || (size gs) <= i || null pattern
    then Nothing
  else if (take (length pattern) $ readPattern gs (i + 1)) == pattern
    then Just $ i + (length pattern) + 1
    else findBackward gs (i - 1) pattern

findMatchTemplate :: Genome -> Int -> SearchDirection -> Maybe Int
findMatchTemplate gs i Forward = if ((size gs) >= i) then Nothing else findForward gs i $ reverseTranscriptase $ readPattern gs (i+1)
findMatchTemplate gs i Backward = if (0 > i) then Nothing else findBackward gs i $ reverseTranscriptase $ readPattern gs (i+1)
findMatchTemplate gs i Outward =
  if isNothing b && isNothing f then Nothing
  else if isNothing b then f
  else if isNothing f then b
  else if (fmap abs $ fmap (flip (-) i) b) < (fmap abs $ fmap (flip (-) i) f) then b else f
  where
    b = findBackward gs i $ reverseTranscriptase pattern
    f = findForward gs i $ reverseTranscriptase pattern
    pattern = readPattern gs (i+1)

jmpo :: Instruction
jmpo i world = modifing foundAdr
  where
    foundAdr = findMatchTemplate (theAnt ^. genome) (theAnt ^. ip) Outward
    modifing Nothing = err i world
    modifing (Just x) = world & ((ants <<< (ix i) <<< ip) .~ (x `mod` (size $ theAnt ^. genome)))
    theAnt = (world ^. ants) ! i

jmpb :: Instruction
jmpb i world = modifing foundAdr
  where
    foundAdr = findMatchTemplate (theAnt ^. genome) (theAnt ^. ip) Backward
    modifing Nothing = err i world
    modifing (Just x) = world & ((ants <<< (ix i) <<< ip) .~ (x `mod` (size $ theAnt ^. genome)))
    theAnt = (world ^. ants) ! i

call :: Instruction
call i world =
  if ip0 == jmpedIP
    then jmped
  else 
    if 10 <= (length $ ((world ^. ants) ! i) ^. stack)
      then err i jmped
      else jmped & (ants <<< ix i <<< stack) %~ ((:) ip0)
  where
    ip0 = ((world ^. ants) ! i) ^. ip
    jmped = jmpo i world
    jmpedIP = ((jmped ^. ants) ! i) ^. ip

getSuger :: Int -> Instruction
getSuger deliciousness i world = world & (ants <<< (ix i) <<< hunger) +~ deliciousness

getSuger0 :: Instruction
getSuger0 = getSuger 10

getAnteater :: Int -> Instruction
getAnteater pain i world = world & (ants <<< (ix i) <<< hunger) -~ pain

getAnteater0 :: Instruction
getAnteater0 = getAnteater 10

-- get coordinates
cdnU :: (Int, Int) -> (Int, Int)
cdnU = id *** (flip (-) 1)
cdnD :: (Int, Int) -> (Int, Int)
cdnD = id *** (+1)
cdnL :: (Int, Int) -> (Int, Int)
cdnL = (flip (-) 1) *** id
cdnR :: (Int, Int) -> (Int, Int)
cdnR = (+1) *** id

-- f is a moving function
-- TODO: 食われた砂糖は消えるように
move :: ((Int, Int) -> (Int, Int)) -> Instruction
move f i world = movingToThe $ ptToObj world p
  where
    theAnt = (world ^. ants) ! i -- 蟻が次の瞬間行く予定の場所の座標
    p :: (Int, Int)
    p = f (theAnt ^. coordinates) -- 蟻が次の瞬間行く予定の場所に元々なんかいたらそいつのobject番号
    
    movingToThe :: ObjectNumber -> GraphPaper
    movingToThe (-1) = err i world
    movingToThe 0 = world & (ants <<< (ix i) <<< coordinates) .~ p -- if 元々居た何か is ｢無｣
    movingToThe 1 = err i world -- if 元々居た何か is 「蟻」
    movingToThe 2 = (movingToThe 0) & (getSuger0 i) -- if 元々居た何か is 「砂糖」
    movingToThe 3 = (movingToThe 0) & (getAnteater0 i) -- if 元々居た何か is 「砂糖」
    movingToThe 4 = err i world -- if 元々居た何か is 「サーバー」

check :: ((Int, Int) -> (Int, Int)) -> Instruction
check f i world = pushToTheStack (ptToObj world (f (theAnt ^. _1))) i world
  where
    theAnt = (world ^. ants) ! i

rand :: Instruction
rand i world = (world & (ants <<< (ix i) <<< register <<< _1) .~ x) & grppStdGen .~ r0
  where
    (x, r0) = randomR (0, abs ax) $ world ^. grppStdGen
    ax = ((world ^. ants) ! i) ^. (register <<< _1)

movdi :: Instruction
movdi i world = if ax < 0 || ax > (size $ theAnt ^. genome) then err i world else world & (ants <<< (ix i) <<< genome <<< (ix ax)) .~ bx
  where
    theAnt = (world ^. ants) ! i
    ax = theAnt ^. (register<<<_1)
    bx = theAnt ^. (register<<<_2)

-- 接触している蟻同士のP2P通信
attack :: ((Int, Int) -> (Int, Int)) -> Instruction
attack f i world = if 1 /= (ptToObj world $ f $ theAnt ^. _1) then err i world else world & (ants <<< (ix (fromJust ixOfAntAttackedByTheAnt)) <<< hunger) -~ 5
  where 
    theAnt = (world ^. ants) ! i
    ixOfAntAttackedByTheAnt :: Maybe Int
    ixOfAntAttackedByTheAnt = elemIndex (f $ theAnt ^. _1) $ map (^. _1) $ elems $ world ^. ants

reward :: ((Int, Int) -> (Int, Int)) -> Instruction
reward f i world = if 1 /= (ptToObj world $ f $ theAnt ^. _1) then err i world else world & (ants <<< (ix (fromJust ixOfAntAttackedByTheAnt)) <<< hunger) +~ 5
  where 
    theAnt = (world ^. ants) ! i
    ixOfAntAttackedByTheAnt :: Maybe Int
    ixOfAntAttackedByTheAnt = elemIndex (f $ theAnt ^. _1) $ map (^. _1) $ elems $ world ^. ants

mention :: ((Int, Int) -> (Int, Int)) -> Instruction
mention f i world =
  if 1 /= (ptToObj world $ f $ theAnt ^. _1) || 0 == (length $ theAnt ^. stack)
    then err i world
    else world & (ants <<< (ix (fromJust ixOfAntMentionedByTheAnt) <<< ear)) %~ ((:) $ head $ theAnt ^. stack) & discard i
  where
    theAnt = (world ^. ants) ! i
    ixOfAntMentionedByTheAnt = elemIndex (f $ theAnt ^. _1) $ map (^. _1) $ elems $ world ^. ants

listen :: Instruction
listen i world = if 10 <= (length (theAnt ^. stack)) || null (theAnt ^. ear) then err i world else pushToTheStack (head $ theAnt ^. ear) i world
  where
    theAnt = (world ^. ants) ! i

nop :: Instruction
nop = flip const

namedInsts1 :: [(Int, String, Instruction)]
namedInsts1 =
  [(0,    "nop0", nop),
    (1,    "nop1", nop),
    (2,    "shl", shl),
    (3,   "zero", zero),
    (4,    "ifz", ifz),
    (5, "subCAB", subCAB),
    (6, "subAAC", subAAC),
    (7,   "incA", incA),
    (8,   "incB", incB),
    (9,   "decC", decC),
    (10,   "incC", incC),
    (11, "pushA", pushA),
    (12, "pushB", pushB),
    (13, "pushC", pushC),
    (14, "pushD", pushD),
    (15,  "popA", popA),
    (16,  "popB", popB),
    (17,  "popC", popC),
    (18,  "popD", popD),
    (19,  "jmpo", jmpo),
    (20,   "mvU", move cdnU),
    (21,   "mvD", move cdnD),
    (22,   "mvL", move cdnL),
    (23,   "mvR", move cdnR),
    (24,"checkU", check cdnU),
    (25,"checkD", check cdnD),
    (26,"checkL", check cdnL),
    (27,"checkR", check cdnR),
    (28,  "rand", rand),
    (29,"rewardU", reward cdnU),
    (30,"rewardD", reward cdnD),
    (31,"rewardL", reward cdnL),
    (32,"rewardR", reward cdnR),
    (33,"mentionU", mention cdnU),
    (34,"mentionD", mention cdnD),
    (35,"mentionL", mention cdnL),
    (36,"mentionR", mention cdnR),
    (37,  "listen", listen)]

insts1 :: InstructionSet
insts1 =
  (array <<< ((,) 0) <<< (flip (-) 1) <<< length) <*> (map removeName) $ namedInsts1
  where
    removeName (n, name, inst) = (n, inst)

namesOfInsts1 :: [String]
namesOfInsts1 = map (^. _2) namedInsts1

numberOfInst1 :: String -> Int
numberOfInst1 = fromJust <<< elemIndex `flip` namesOfInsts1

-- アセンブル
asmOfInsts1 :: [String] -> [Int]
asmOfInsts1 = map numberOfInst1

nameOfInst1 :: Int -> String
nameOfInst1 = fromJust <<< ((^?) namesOfInsts1) <<< ix

-- 逆アセンブル
unasmOfInsts1 :: [Int] -> [String]
unasmOfInsts1 = map nameOfInst1

-- 添字付きで逆アセンブル
indexedUnasmOfInsts1 :: [Int] -> [(Int, String)]
indexedUnasmOfInsts1 = (zip [0..]) <<< unasmOfInsts1
