-- 参考: http://coderepos.org/share/browser/lang/csharp/Tierra/trunk 
{-# LANGUAGE Rank2Types #-}
module Instruct where

import Control.Lens
import Control.Arrow
import Control.Monad
import Control.Applicative
import Data.Array
import Data.Bits
import Data.List
import Data.List.Zipper
import System.Random
import Data.Maybe

import Engine

getTheAnt :: (Ant -> Instruction) -> Instruction
getTheAnt f world = f (world ^. (ants <<< focus)) world

modifyTheAnt's x f = (ants <<< focus <<< x) %~ f

setTheAnt's x a = (ants <<< focus <<< x) .~ a

shl :: Instruction
shl = modifyTheAnt's (register <<< _3) $ flip shiftL 1

zero :: Instruction
zero = setTheAnt's (register <<< _3) 0

ifz :: Instruction
ifz world = if cx == 0 then world else incIP world
  where cx = (world ^. (ants <<< focus)) ^. (register <<< _3)

subCAB :: Instruction
subCAB world = world & ((ants <<< focus <<< register <<< _3) .~ (ax - bx))
  where
    ax = reg ^. _1
    bx = reg ^. _2
    reg = world ^. (ants <<< focus <<< register)

subAAC :: Instruction
subAAC world = world & (ants <<< focus <<< register <<< _1) -~ cx
  where
    cx = reg ^. _1
    reg = world ^. (ants <<< focus <<< register)

incA :: Instruction
incA = modifyTheAnt's (register <<< _1) succ

incB :: Instruction
incB = modifyTheAnt's (register <<< _2) succ

decC :: Instruction
decC = modifyTheAnt's (register <<< _3) pred

incC :: Instruction
incC = modifyTheAnt's (register <<< _3) succ

-- 手続きの実行が失敗した時などに減点する
err:: Instruction
err = modifyTheAnt's hunger $ pred

pushToTheStack :: Int -> Instruction
pushToTheStack x world =
  if (length xs) >= 10
    then err world
    else world & ((ants <<< focus <<< stack) %~ ((:) x))
  where
    xs = world ^. (ants <<< focus <<< stack)

pushA :: Instruction
pushA world =
  if (length xs) >= 10 
    then err world
    else world & ((ants <<< focus <<< stack) %~ ((:) ax))
  where
    ax = world ^. (ants <<< focus <<< register <<< _1)
    xs = world ^. (ants <<< focus <<< stack)

pushB :: Instruction
pushB world =
  if (length xs) >= 10 
    then err world
    else world & ((ants <<< focus <<< stack) %~ ((:) bx))
  where
    bx = world ^. (ants <<< focus <<< register <<< _2)
    xs = world ^. (ants <<< focus <<< stack)

pushC :: Instruction
pushC world =
  if (length xs) >= 10 
    then err world
    else world & ((ants <<< focus <<< stack) %~ ((:) cx))
  where
    cx = world ^. (ants <<< focus <<< register <<< _3)
    xs = world ^. (ants <<< focus <<< stack)

pushD :: Instruction
pushD world =
  if (length xs) >= 10 
    then err world
    else world & ((ants <<< focus <<< stack) %~ ((:) cx))
  where
    cx = world ^. (ants <<< focus <<< register <<< _4)
    xs = world ^. (ants <<< focus <<< stack)

-- スタックの一番上の値を破棄する
discard :: Instruction
discard = modifyTheAnt's stack tail

popA :: Instruction
popA world =
  if null $ ((world ^. ants) ^. focus) ^. stack then err world
    else (discard world) & (ants <<< focus <<< register <<< _1) .~ x
  where
    x = head $ world ^. (ants <<< focus <<< stack)

popB :: Instruction
popB world =
  if null $ ((world ^. ants) ^. focus) ^. stack then err world
    else (discard world) & (ants <<< focus <<< register <<< _2) .~ x
  where
    x = head $ world ^. (ants <<< focus <<< stack)

popC :: Instruction
popC world = 
  if null $ ((world ^. ants) ^. focus) ^. stack then err world
    else (discard world) & (ants <<< focus <<< register <<< _3) .~ x
  where
    x = head $ world ^. (ants <<< focus <<< stack)

popD :: Instruction
popD world =
  if null $ ((world ^. ants) ^. focus) ^. stack then err world
    else (discard world) & (ants <<< focus <<< register <<< _4) .~ x
  where
    x = head $ world ^. (ants <<< focus <<< stack)

readPattern :: Genome -> Int -> [Bool]
readPattern gs i
  | (size gs) <= i || i < 0 = []
  | (gs ! i) == 0           = False : readPattern gs (i + 1) 
  | (gs ! i) == 1           = True : readPattern gs (i + 1)
  | otherwise               = []

reverseTranscriptase :: [Bool] -> [Bool]
reverseTranscriptase = map not

findForward :: Genome -> Int -> [Bool] -> Maybe Int
findForward gs i pattern =
  if (size gs) <= i || i < 0 || null pattern
    then Nothing
  else if (take (length pattern) $ readPattern gs (i + 1)) == pattern
    then Just $ i + (length pattern) + 1
    else findForward gs (i + 1) pattern

findBackward :: Genome -> Int -> [Bool] -> Maybe Int
findBackward gs i pattern = 
  if i < 0 || (size gs) <= i || null pattern
    then Nothing
  else if (take (length pattern) $ readPattern gs (i + 1)) == pattern
    then Just $ i + (length pattern) + 1
    else findBackward gs (i - 1) pattern

findMatchForward :: Genome -> Int -> Maybe Int
findMatchForward gs i = findBackward gs i $ reverseTranscriptase $ readPattern gs (i+1)

findMatchBackward :: Genome -> Int -> Maybe Int
findMatchBackward gs i = findForward gs i $ reverseTranscriptase $ readPattern gs (i+1)

findMatchOutward :: Genome -> Int -> Maybe Int
findMatchOutward gs i =
  if isNothing b && isNothing f then Nothing
  else if isNothing b then f
  else if isNothing f then b
  else if (fmap abs $ fmap (flip (-) i) b) < (fmap abs $ fmap (flip (-) i) f) then b else f
  where
    b = findBackward gs i $ reverseTranscriptase pattern
    f = findForward gs i $ reverseTranscriptase pattern
    pattern = readPattern gs (i+1)

jmpo :: Instruction
jmpo world = modifing foundAdr
  where
    foundAdr = findMatchOutward (theAnt ^. genome) (theAnt ^. ip)
    modifing Nothing = err world
    modifing (Just x) = world & ((ants <<< focus <<< ip) .~ (x `mod` (size $ theAnt ^. genome)))
    theAnt = world ^. (ants <<< focus)

jmpb :: Instruction
jmpb world = modifing foundAdr
  where
    foundAdr = findMatchBackward (theAnt ^. genome) (theAnt ^. ip)
    modifing Nothing = err world
    modifing (Just x) = world & ((ants <<< focus <<< ip) .~ (x `mod` (size $ theAnt ^. genome)))
    theAnt = world ^. (ants <<< focus)

call :: Instruction
call world =
  if ip0 == jmpedIP
    then jmped
  else 
    if 10 <= (length $ ((world ^. ants) ^. focus) ^. stack)
      then err jmped
      else jmped & (ants <<< focus <<< stack) %~ ((:) ip0)
  where
    ip0 = world ^. (ants <<< focus <<< ip)
    jmped = jmpo world
    jmpedIP = jmped ^. (ants <<< focus <<< ip)

getSuger :: Int -> Instruction
getSuger deliciousness world = world & (ants <<< focus <<< hunger) +~ deliciousness

getSuger0 :: Instruction
getSuger0 = getSuger 10

getAnteater :: Int -> Instruction
getAnteater pain world = world & (ants <<< focus <<< hunger) -~ pain

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

delByCoords :: (Object a) => (Lens' GraphPaper [a]) -> (Int, Int) -> Instruction
delByCoords obj p = obj %~ (filter ((/= p) <<< (^. coords)))

-- f is a moving function
move :: ((Int, Int) -> (Int, Int)) -> Instruction
move f world = movingToThe $ ptToObj world p
  where
    theAnt = (world ^. ants) ^. focus -- 蟻が次の瞬間行く予定の場所の座標
    p :: (Int, Int)
    p = f (theAnt ^. coords) -- 蟻が次の瞬間行く予定の場所に元々なんかいたらそいつのobject番号
    
    movingToThe :: ObjectNumber -> GraphPaper
    movingToThe (-1) = err world
    movingToThe 0 = world & (ants <<< focus <<< coords) .~ p -- if 元々居た何か is ｢無｣
    movingToThe 1 = err world -- if 元々居た何か is 「蟻」
    movingToThe 2 = ((movingToThe 0) & getSuger0) & (delByCoords sugers p)-- if 元々居た何か is 「砂糖」
    movingToThe 3 = ((movingToThe 0) & getAnteater0) & (delByCoords anteaters p)-- if 元々居た何か is 「砂糖」
    movingToThe 4 = err world -- if 元々居た何か is 「サーバー」

check :: ((Int, Int) -> (Int, Int)) -> Instruction
check f world = pushToTheStack (ptToObj world (f (theAnt ^. coords))) world
  where
    theAnt = (world ^. ants) ^. focus

rand :: Instruction
rand world = (world & (ants <<< focus <<< register <<< _1) .~ x) & gen .~ r0
  where
    (x, r0) = randomR (0, abs ax) $ world ^. gen
    ax = world ^. (ants <<< focus <<< register <<< _1)

movdi :: Instruction
movdi world = if ax < 0 || ax > (size $ theAnt ^. genome) then err world else world & (ants <<< focus <<< genome <<< (ix ax)) .~ bx
  where
    theAnt = (world ^. ants) ^. focus
    ax = theAnt ^. (register<<<_1)
    bx = theAnt ^. (register<<<_2)

antLensByPt :: (Int, Int) -> (Lens' (Zipper Ant) Ant)
antLensByPt p = lens get set
  where
    get :: (Zipper Ant) -> Ant
    get (Zip ls rs) =
      if isJust $ elemIndex p (map (^. coords) ls)
        then fromJust $ ls ^? (ix $ fromJust $ elemIndex p (map (^. coords) ls))
        else fromJust $ rs ^? (ix $ fromJust $ elemIndex p (map (^. coords) rs))
    set :: (Zipper Ant) -> Ant -> (Zipper Ant)
    set (Zip ls rs) b = 
      if isJust $ elemIndex p (map (^. coords) ls)
        then Zip (ls & ((ix $ fromJust $ elemIndex p (map (^. coords) ls)) .~ b)) rs
        else Zip ls $ rs & (ix $ fromJust $ elemIndex p (map (^. coords) rs)) .~ b

-- 接触している蟻同士のP2P通信
attack :: ((Int, Int) -> (Int, Int)) -> Instruction
attack f world = if 1 /= (ptToObj world $ f $ theAnt ^. coords) then err world else world & (ants <<< (antLensByPt (f $ theAnt ^. coords)) <<< hunger) -~ 5
  where 
    theAnt = (world ^. ants) ^. focus

reward :: ((Int, Int) -> (Int, Int)) -> Instruction
reward f world = if 1 /= (ptToObj world $ f $ theAnt ^. coords) then err world else world & (ants <<< (antLensByPt $ f $ theAnt ^. coords) <<< hunger) +~ 5
  where 
    theAnt = (world ^. ants) ^. focus

mention :: ((Int, Int) -> (Int, Int)) -> Instruction
mention f world =
  if 1 /= (ptToObj world $ f $ theAnt ^. coords) || 0 == (length $ theAnt ^. stack)
    then err world
    else world & (ants <<< (antLensByPt $ f $ theAnt ^. coords) <<< ear) %~ ((:) $ head $ theAnt ^. stack) & discard
  where
    theAnt = (world ^. ants) ^. focus

listen :: Instruction
listen world = if 10 <= (length (theAnt ^. stack)) || null (theAnt ^. ear) then err world else pushToTheStack (head $ theAnt ^. ear) world
  where
    theAnt = (world ^. ants) ^. focus

namedInsts1 :: [(Int, String, Instruction)]
namedInsts1 =
  [(0,    "nop0", id),
    (1,    "nop1", id),
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

instSet :: [(Int, String, Instruction)] -> InstructionSet
instSet =
  (array <<< ((,) 0) <<< (flip (-) 1) <<< length) <*> (map ((^. _1) &&& (^. _3)))

nameSet :: [(Int, String, Instruction)] -> [String]
nameSet = map (^. _2)

numberOfInst :: [(Int, String, Instruction)] -> String -> Int
numberOfInst = (fromJust <<<) <<< (flip elemIndex) <<< nameSet

-- アセンブル
asm :: [(Int, String, Instruction)] -> [String] -> [Int]
asm = map <<< numberOfInst

nameOfInst :: [String] -> Int -> String
nameOfInst = flip $ (fromJust <<<) <<< (flip (^?)) <<< ix

-- 逆アセンブル
unasm :: [String] -> [Int] -> [String]
unasm = map <<< nameOfInst

-- 添字付きで逆アセンブル
indexedUnasm :: [String] -> [Int] -> [(Int, String)]
indexedUnasm = (zip [0..] <<<) <<< unasm
