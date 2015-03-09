-- 参考: http://coderepos.org/share/browser/lang/csharp/Tierra/trunk 

module ScientificAnts.InstructionSet where

import Control.Lens
import Control.Arrow
import Control.Monad
import Data.Array
import Data.Bits
import Data.List

import ScientificAnts.Simulation

data SearchDirection = Forward | Backward | Outward

shl :: Instruction
shl i world = world & ((ants <<< (ix i) <<< register <<< _3) %~ (flip shiftL 1))

zero :: Instruction
zero i world = world & ((ants <<< (ix i) <<< register <<< _3) .~ 0)

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
subAAC i world = world & ((ants <<< (ix i) <<< register <<< _1) %~ (flip (-) cx))
  where
    cx = (reg ^. _1)
    reg = ((world ^. ants) ! i) ^. register

incA :: Instruction
incA i world = world & ((ants <<< (ix i) <<< register <<< _1) %~ (+1))

incB :: Instruction
incB i world = world & ((ants <<< (ix i) <<< register <<< _2) %~ (+1))

decC :: Instruction
decC i world = world & ((ants <<< (ix i) <<< register <<< _3) %~ flip (-) 1)

incC :: Instruction
incC i world = world & ((ants <<< (ix i) <<< register <<< _3) %~ (+1))

-- 手続きの実行が失敗した時などに減点する
err:: Instruction
err i world = world & ((ants <<< (ix i) <<< hunger) %~ flip (-) 1)

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
discard i world = world & (ants <<< (ix i) <<< stack) %~ tail

popA :: Instruction
popA i world =
  if (((world ^. ants) ! i) ^. stack) == [] then err i world
    else (discard i world) & (ants <<< (ix i) <<< register <<< _1) .~ x
  where
    x = head (((world ^. ants) ! i) ^. stack)

popB :: Instruction
popB i world =
  if (((world ^. ants) ! i) ^. stack) == [] then err i world
    else (discard i world) & (ants <<< (ix i) <<< register <<< _2) .~ x
  where
    x = head (((world ^. ants) ! i) ^. stack)

popC :: Instruction
popC i world = 
  if (((world ^. ants) ! i) ^. stack) == [] then err i world
    else (discard i world) & (ants <<< (ix i) <<< register <<< _3) .~ x
  where
    x = head (((world ^. ants) ! i) ^. stack)

popD :: Instruction
popD i world =
  if (((world ^. ants) ! i) ^. stack) == [] then err i world
    else (discard i world) & (ants <<< (ix i) <<< register <<< _4) .~ x
  where
    x = head (((world ^. ants) ! i) ^. stack)
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
    rt _ = error "reverseTranscriptasing list must be constructed by 0 or 1."

findForward :: Genome -> Int -> [Int] -> Maybe Int
findForward gs i pattern =
  if (size gs) <= i || i < 0 || pattern == []
    then Nothing
  else if (take (length pattern) $ readPattern gs (i + 1)) == pattern
    then Just $ i + (length pattern) + 1
    else findForward gs (i + 1) pattern

findBackward :: Genome -> Int -> [Int] -> Maybe Int
findBackward gs i pattern = 
  if i < 0 || (size gs) <= i || pattern == []
    then Nothing
  else if (take (length pattern) $ readPattern gs (i + 1)) == pattern
    then Just $ i + (length pattern) + 1
    else findBackward gs (i - 1) pattern

findMatchTemplate :: Genome -> Int -> SearchDirection -> Maybe Int
findMatchTemplate gs i Forward = if ((size gs) >= i) then Nothing else findForward gs i $ reverseTranscriptase $ readPattern gs (i+1)
findMatchTemplate gs i Backward = if (0 > i) then Nothing else findBackward gs i $ reverseTranscriptase $ readPattern gs (i+1)
findMatchTemplate gs i Outward =
  if b == Nothing && f == Nothing then Nothing
  else if (b == Nothing) then f
  else if (f == Nothing) then b
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
insts1 :: InstructionSet
insts1 =
  ((listArray & uncurry) <<< ((const 0 &&& ((flip (-) 1) <<< length)) &&& id))
    [nop, nop,   shl,   zero,  ifz,   subCAB, subAAC, incA, incB, decC,
    incC, pushA, pushB, pushC, pushD, popA,   popB,   popC, popD]
  where
    nop :: Instruction
    nop = flip const
