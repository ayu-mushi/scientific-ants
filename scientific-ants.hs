{- 科学者社会のシミュレータ
 - 蟻(エージェント)・砂糖・アリジゴクが在る世界
 - 砂糖とアリジゴクの並びには規則性が在る
 - そこで蟻は、規則性に関する知識を得るために動く
 - 知識は、サーバーを介した通信・蟻同士のp2p通信に因って他の蟻に伝達される
 -}

import Control.Lens
import Data.Array
import Control.Arrow
import Data.Bits
import Data.List
import System.Random
import Control.Monad
import Control.Applicative
import Data.Maybe
import Data.Array.ST

type IP = Int
type Register = (Int, Int, Int, Int)
type Stack = [Int]
type Hunger = Int -- 満腹度
type Genom = Array Int Int

type Ant = ((Int, Int), IP, Register, Stack, Hunger, Genom) -- 「蟻」
ip :: Lens' Ant IP
ip = _2
register :: Lens' Ant Register
register = _3
genom :: Lens' Ant Genom
genom = _6
hunger :: Lens' Ant Hunger
hunger = _5
stack :: Lens' Ant Stack
stack = _4

type Suger = (Int, Int) -- 「砂糖」
type Anteater = (Int, Int) -- 「アリジゴク」、英訳分からないしAnteater(アリクイ)でいいかって流れ(アリクイだと動きそうだけど実際は動きませんしただのアイテムです)
type Server = ((Int, Int), Array Int Int) -- 「サーバー」

-- 方眼紙、格子状の平面、セル・オートマトンのやつ
type GraphPaper = (Array Int Ant, [Suger], [Anteater], [Server])
ants :: Lens' GraphPaper (Array Int Ant)
ants = _1

type Instruction = Int -> GraphPaper -> GraphPaper
type InstructionSet = Array Int Instruction

size :: Array Int a -> Int
size = bounds >>> (^. _2) >>> (+ 1)

incIP :: Instruction
incIP i world =
    world & ((ants <<< (ix i) <<< ip)
      %~ ((+1) >>> (flip mod $ size $ ((world ^. ants) ! i) ^. genom)))

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

pushA :: Instruction
pushA i world = world & ((ants <<< (ix i) <<< stack) %~ push ax)
  where
    ax = ((world ^. ants) ! i) ^. (register <<< _1)
    push x xs = if (length xs) >= 10 then xs else x:xs

pushB :: Instruction
pushB i world = world & ((ants <<< (ix i) <<< stack) %~ push bx)
  where
    bx = ((world ^. ants) ! i) ^. (register <<< _2)
    push x xs = if (length xs) >= 10 then xs else x:xs

pushC :: Instruction
pushC i world = world & ((ants <<< (ix i) <<< stack) %~ push cx)
  where
    cx = ((world ^. ants) ! i) ^. (register <<< _3)
    push x xs = if (length xs) >= 10 then xs else x:xs

pushD :: Instruction
pushD i world = world & ((ants <<< (ix i) <<< stack) %~ push dx)
  where
    dx = ((world ^. ants) ! i) ^. (register <<< _4)
    push x xs = if (length xs) >= 10 then xs else x:xs

-- スタックの一番上の値を破棄する
discard :: Instruction
discard i world = world & (ants <<< (ix i) <<< stack) %~ tail

popA :: Instruction
popA i world = (discard i world) & (ants <<< (ix i) <<< register <<< _1) .~ x
  where
    x = head (((world ^. ants) ! i) ^. stack)

popB :: Instruction
popB i world = (discard i world) & (ants <<< (ix i) <<< register <<< _2) .~ x
  where
    x = head (((world ^. ants) ! i) ^. stack)

popC :: Instruction
popC i world = (discard i world) & (ants <<< (ix i) <<< register <<< _3) .~ x
  where
    x = head (((world ^. ants) ! i) ^. stack)

popD :: Instruction
popD i world = (discard i world) & (ants <<< (ix i) <<< register <<< _4) .~ x
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

-- 時間の齣(コマ)を1つ進める
refreshGraphPaper:: InstructionSet -> (Int, GraphPaper) -> (Int, GraphPaper)
refreshGraphPaper insts (i, world) =
  ((i + 1) `mod` (size (world ^. ants)),
    (incIP i $ (insts ! ((theAnt ^. genom) ! (theAnt ^. ip))) i world))

    where
      theAnt = (world ^. ants) ! i

mutate :: Int -> (Genom, StdGen) -> (Genom, StdGen)
mutate sizeOfInsts (gm, r0) = (gm // [(i, g)], r2)
  where
    (i, r1) = randomR (0, ((size gm) - 1)) r0
    (g, r2) = randomR (0, sizeOfInsts-1) r1

mkAnt :: (Int, Int) -> Genom -> Ant
mkAnt coordinates g = (coordinates, 0, (0, 0, 0, 0), [], 0, g)

-- 選択、満腹度最高よりn匹のゲノム
choise :: Int -> Array Int Ant -> [Genom]
choise n ans = map (^. genom) (folddArr selector ((size ans) - 1) ([] :: [Ant]) ans)
  where
    folddArr :: (a -> b -> a) -> Int -> a -> Array Int b -> a
    folddArr _ 0 a _ = a -- """fold down""" for Array
    folddArr f i a arr = folddArr f (i-1) (f a (arr ! i)) arr
    
    selector :: [Ant] -> Ant -> [Ant]
    selector [] a = [a]
    selector xs a = if (length xs) < n
      then insert a xs
      else (if (all ((^. hunger) >>> (> a ^. hunger)) xs) then xs else init (insert a xs))

fpow :: (a -> a) -> Int -> (a -> a)
fpow _ 0 = id
fpow f n = f . (fpow f (n - 1))

randmap :: ((a, StdGen) -> (b, StdGen)) -> ([a], StdGen) -> ([b], StdGen)
randmap _ ([], r0) = ([], r0)
randmap f ((x:xs), r0) = ((y:ys), r2)
  where
    (ys, r2) = randmap f (xs, r1)
    (y, r1) = f (x, r0)

flatArray :: [Array Int a] -> Array Int a
flatArray arys = listArray (0, length xss-1) $ xss
  where
    xss = concat $ map elems arys

mapWithIx :: (a -> Int -> b) -> [a] -> [b]
mapWithIx = mapWithIx' 0
  where
    mapWithIx' _ _ [] = []
    mapWithIx' i f (x:xs) = (f x i : mapWithIx' (i+1) f xs)

slice :: Int -> [a] -> [[a]]
slice n = unfoldr phi
  where
    phi [] = Nothing
    phi xs = Just $ splitAt n xs

replicateWithRemainder :: (Int, Int) -> [a] -> [[a]]
replicateWithRemainder (n, 0) xs = replicate n xs
replicateWithRemainder (n, remainder) xs = concat [(replicate n xs),[(take remainder xs)]]

-- 選ばれた強い蟻から、numOfAnts匹の蟻を作る
nextGeneration :: [(Int, Int)] -> Int -> Float -> StdGen -> [Genom] -> Array Int Ant
nextGeneration spacings sizeOfInsts mutationalRate r0 genoms =
  if numOfAnts == 0
    then listArray (0, 0) []
  else if (length genoms) >= numOfAnts
    then listArray (0, numOfAnts-1) $
      take numOfAnts $
        (mapWithIx (mkAnts spacings)) $ (^. _1) $
          (randmap 
            (fpow (mutate sizeOfInsts) $ round $ ((randomR (0.0, fromIntegral $ length genoms) r0) ^. _1) / mutationalRate)
            (genoms, r0))
    else 
      flatArray $
        zipWith (nextGeneration `flip` sizeOfInsts `flip` mutationalRate `flip` r0)
          (slice (length genoms) spacings)
          $ replicateWithRemainder (numOfAnts `divMod` (length genoms)) genoms 

  where
    mkAnts :: [(Int, Int)] -> Genom -> Int -> Ant
    mkAnts xys g i = mkAnt (certainsure (xys ^? ix i)) g
    numOfAnts = length spacings

    -- 確実に(たぶん)
    certainsure :: Maybe (Int, Int) -> (Int, Int)
    certainsure Nothing = error "Nothing"
    certainsure (Just x) = x

mkServer :: (Int, Int) -> Server
mkServer coordinates = (coordinates, array (0, 10) [])

ancestor :: Genom
ancestor = array (0,3) [(0, 0), (1, 1), (2, 1), (3, 4)]

type ObjectNumber = Int
-- 0 - 「無」、1 - 「蟻」、2 - 「砂糖」、3 - 「アリジゴク」、4 - 「サーバー」

wall :: [Int] -> Int -> ObjectNumber
wall = wall' 0 
  where
    wall' :: Int -> [Int] -> Int -> Int
    wall' i [x] p = i
    wall' i (x:(y:xs)) p = if p < x then i else wall' (i + 1) ((x + y) : xs) p

--relativize :: [Int] -> [Int]
--relativeze = 

-- アイテム/エージェントの配置を決める
spacingObjects ::
  StdGen ->
  [Int -> Int] -- 各々の"傾向"を表す函数列 //傾向性 is 未実装な
    -> [Int] -- 各アイテム/エージェントの個数を表す列
    -> Int -> Int -- GraphPaperの幅と高さ
    -> Array (Int,Int) ObjectNumber -- アイテム/エージェントの配置
spacingObjects r0 fs ns width height = array ((0, 0), (width-1, height-1)) $ spacingObjects' r0 ns 0 0 [] 
  where 
    spacingObjects' :: StdGen -> [Int] -> Int -> Int -> [((Int, Int), ObjectNumber)] -> [((Int, Int), ObjectNumber)]
    spacingObjects' r0 ns i j xs =
      if j == height
        then
          []
        else
          ((i, j), obj)
          : (if width /= (i + 1)
            then spacingObjects' r1 (ns & (ix obj) %~ (flip (-) 1)) (i+1) j xs
            else spacingObjects' r1 (ns & (ix obj) %~ (flip (-) 1)) 0 (j+1) xs)
      where
        (x, r1) = randomR (0, ((sum ns) - 1)) r0
        distances = map (abs <<< ((-) j) <<< ($ i)) fs
        obj = wall ns x

spacingOfEachObjects :: StdGen -> [Int -> Int] -> [Int] -> Int -> Int -> [[(Int, Int)]]
spacingOfEachObjects r0 fs ns width height =
  [[(x, y)
    | x <- [0..width-1], y <- [0..height-1], (aryDim2 ! (x, y)) == i]
      | i <- [0..(length ns)-1]]
  where
    aryDim2 = spacingObjects r0 fs ns width height 

world1 :: StdGen -> GraphPaper
world1 r0 = toGrPp tuplizedSOEO
    where
      soeo = spacingOfEachObjects r0 [id] [5,1,1,1,1] 3 3
      take4 :: [a] -> (a, a, a, a)
      take4 (x:(y:(z:(w:_)))) = (x, y, z, w)
      tuplizedSOEO = take4 soeo
      toGrPp (x, y, z, w) =
        (nextGeneration x (size insts1) 0.05 r0 (replicate 5 ancestor),
          y,
          z,
          map mkServer w)

main = print $ nextGeneration [(4,6),(3,5),(3,5)] (size insts1) 0.05 (mkStdGen 100) (replicate 2 ancestor)
