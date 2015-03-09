module ScientificAnts.InstructionSet where

import Control.Lens
import Control.Arrow
import Control.Monad
import Data.Array
import Data.Bits
import Data.List

import ScientificAnts.Simulation

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
    reg = (world ^. ants) ! i ^. register

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

insts1 :: InstructionSet
insts1 =
  ((listArray & uncurry) <<< ((const 0 &&& length) &&& id))
    [nop, nop,   shl,   zero,  ifz,   subCAB, subAAC, incA, incB, decC,
    incC, pushA, pushB, pushC, pushD, popA,   popB,   popC, popD]
  where
    nop :: Instruction
    nop = flip const
