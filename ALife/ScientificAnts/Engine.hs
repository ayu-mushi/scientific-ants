{-# Language TemplateHaskell #-}

module ALife.ScientificAnts.Engine where

import GHC.Read
import qualified Text.Read.Lex as L
import Text.ParserCombinators.ReadPrec

import Control.Lens
import Data.Array
import Control.Arrow
import Data.List
import System.Random
import Control.Monad
import Control.Applicative
import Data.Maybe
import Data.Array.ST
import Data.Ratio
import Data.List.Zipper

class Object a where -- 空間上の或る一点を占める事ができる存在
  coords :: Lens' a (Int, Int)

type Register = (Int, Int, Int, Int)
type Stack = [Int]
type Genome = Array Int Int

data Ant = Ant -- 「蟻」
  { _coordinates :: (Int, Int)
  , _ip :: Int
  , _register :: Register
  , _genome :: Genome
  , _hunger :: Int -- 満腹度
  , _stack :: Stack
  , _ear :: Stack
  } deriving (Show, Read)
makeLenses ''Ant
instance Object Ant where
  coords = coordinates

newtype Suger = Suger (Int, Int) deriving (Show, Read)
instance Object Suger where
  coords = lens (\(Suger p) -> p) (\(Suger p) q -> (Suger q))

newtype Anteater = Anteater (Int, Int) deriving (Show, Read)
instance Object Anteater where
  coords = lens (\(Anteater p) -> p) (\(Anteater p) q -> (Anteater q))

data Server = Server (Int, Int) (Array Int Int) deriving (Show, Read)
instance Object Server where
  coords = lens (\(Server p _) -> p) (\(Server p a) q -> (Server q a))

instance (Read a) => Read (Zipper a) where
  readPrec =
    parens $ prec appPrec $
      do
        L.Ident "Zip" <- lexP
        ls <- step readPrec
        rs <- step readPrec
        return (Zip ls rs)
      where
        appPrec = 3

-- 方眼紙、格子状の平面、セル・オートマトンのやつ
data GraphPaper = GraphPaper
  { _ants :: Zipper Ant
  , _sugers :: [Suger]
  , _anteaters :: [Anteater]
  , _servers :: [Server]
  , _width :: Int
  , _height :: Int
  , _gen :: StdGen
  } deriving (Show, Read)
makeLenses ''GraphPaper

type Instruction = GraphPaper -> GraphPaper
type InstructionSet = Array Int Instruction

size :: Array Int a -> Int
size = bounds >>> (^. _2) >>> (+ 1)

focus :: Lens' (Zipper a) a
focus = lens cursor $ flip replace

incIP :: Instruction
incIP world =
    world & (ants <<< focus <<< ip)
      %~ ((+1) >>> (flip mod $ size $ world ^. (ants <<< focus <<< genome)))

begin :: Zipper a -> Zipper a
begin (Zip ls rs) = Zip (ls ++ rs) []

-- 時間の齣(コマ)を1つ進める
refresh:: InstructionSet -> GraphPaper -> GraphPaper
refresh insts world =
  if endp (world ^. ants)
    then world & execute & ants %~ begin
    else world & execute & ants %~ Data.List.Zipper.left

    where
      execute = incIP . (insts ! ((theAnt ^. genome) ! (theAnt ^. ip)))
      theAnt = (world ^. ants) ^. focus 

mutate :: Int -> (Genome, StdGen) -> (Genome, StdGen)
mutate sizeOfInsts (gm, r0) =
  if i == (size gm)
    then (listArray (0, size gm) (g : (elems gm)), r2)
    else if i == -1
      then (if 1 >= (size gm) then (gm, r2) else (listArray (0, (size gm) - 2) (tail (elems gm)), r2))
      else (gm // [(i, g)], r2)
  where
    (i, r1) = randomR (-1, size gm) r0
    (g, r2) = randomR (0, (sizeOfInsts - 1)) r1

mkAnt :: (Int, Int) -> Genome -> Ant
mkAnt cs g = Ant {_coordinates = cs, _ip = 0, _register = (0, 0, 0, 0), _genome = g, _hunger = 0, _stack = [], _ear = []}

instance Eq Ant where
  a == b = (a ^. hunger) == (b ^. hunger)

instance Ord Ant where
  a `compare` b = (a ^. hunger) `compare` (b ^. hunger)

-- 選択、満腹度最高よりn匹のゲノム
choise :: Int -> [Ant] -> [Genome]
choise n as = map (^. genome) $ foldl' selector [] as
  where
    selector :: [Ant] -> Ant -> [Ant]
    selector [] a = [a]
    selector xs a = if (length xs) < n
      then Data.List.insert a xs
      else (if (all (> a) xs) then xs else init (Data.List.insert a xs))

fpow :: (a -> a) -> Int -> (a -> a)
fpow _ 0 = id
fpow f n = f . (fpow f (n - 1))

randmap :: ((a, StdGen) -> (b, StdGen)) -> ([a], StdGen) -> ([b], StdGen)
randmap _ ([], r0) = ([], r0)
randmap f ((x:xs), r0) = ((y:ys), r2)
  where
    (ys, r2) = randmap f (xs, r1)
    (y, r1) = f (x, r0)

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
nextGeneration :: [(Int, Int)] -> Int -> Float -> StdGen -> [Genome] -> [Ant]
nextGeneration disposings sizeOfInsts mutationalRate r0 genomes =
  if numOfAnts == 0
    then []
  else if (length genomes) >= numOfAnts
    then 
      take numOfAnts $
        (mapWithIx (mkAnts disposings)) $ (^. _1) $
          (randmap 
            (fpow (mutate sizeOfInsts) $ round $ ((randomR (0.0, fromIntegral $ length genomes) r0) ^. _1) / mutationalRate)
            (genomes, r0))
    else
      concat $ zipWith (nextGeneration `flip` sizeOfInsts `flip` mutationalRate `flip` r0)
        (slice (length genomes) disposings)
          $ replicateWithRemainder (numOfAnts `divMod` (length genomes)) genomes 

  where
    mkAnts :: [(Int, Int)] -> Genome -> Int -> Ant
    mkAnts xys g i = mkAnt (fromJust (xys ^? ix i)) g
    numOfAnts = length disposings

mkServer :: (Int, Int) -> Server
mkServer p = Server p $ listArray (0, 10) (replicate 11 0)

type ObjectNumber = Int
-- 0 - 「無」、1 - 「蟻」、2 - 「砂糖」、3 - 「アリジゴク」、4 - 「サーバー」

wall :: [Int] -> Int -> ObjectNumber
wall = wall' 0 
  where
    wall' :: ObjectNumber -> [Int] -> Int -> ObjectNumber
    wall' i [x] p = i
    wall' i (x:(y:xs)) p = if p < x then i else wall' (i + 1) ((x + y) : xs) p

-- アイテム/エージェントの配置を決める
disposeObjs ::
  StdGen ->
  [[(Int,Int)]] -- 各々の"傾向"を表す二次元ベクトルの列の列
    -> [Int] -- 各アイテム/エージェントの個数を表す列
    -> Int -- GraphPaperの幅
    -> Array (Int,Int) ObjectNumber -- アイテム/エージェントの配置
disposeObjs r0 vss ns width = array ((0, 0), (width-1, height-1)) $ disposeObjs' r0 ns 0 0 []
  where
    height = (sum ns) `div` width
    disposeObjs' :: StdGen -> [Int] -> Int -> Int -> [((Int, Int), ObjectNumber)] -> [((Int, Int), ObjectNumber)]
    disposeObjs' r0 ns i j xs =
      if j == height
        then
          []
        else
          ((i, j), obj)
          : (if width /= (i + 1)
            then disposeObjs' r1 (ns & (ix obj) -~ 1) (i+1) j xs
            else disposeObjs' r1 (ns & (ix obj) -~ 1) 0 (j+1) xs)
      where
        weight = map (\x -> if (i,j) `elem` x then 2 else 1) vss
        weightedNs = zipWith (*) ns weight
        (x, r1) = randomR (0, (sum weightedNs) - 1) r0
        obj = wall weightedNs x

ptToObj :: GraphPaper -> (Int, Int) -> ObjectNumber
ptToObj world p
  | (p ^. _1) < 0 || (p ^. _2) < 0 || (world ^. width) <= (p ^. _1) ||  (world ^. height) <= (p ^. _2) = -1
  | p `elem` (map (^. coords) $ toList $ world ^. ants)       =  1
  | p `elem` (map (^. coords) (world ^. sugers))              =  2
  | p `elem` (map (^. coords) (world ^. anteaters))           =  3
  | p `elem` (map (^. coords) (world ^. servers))             =  4
  | otherwise                                                 =  0

print2dArray :: (Show a) => Array (Int, Int) a -> String
print2dArray ary2d = print2dArray' ary2d 0 0
  where
    print2dArray' :: (Show a) => Array (Int, Int) a -> Int -> Int -> String
    print2dArray' ary2d i j = 
      (show (ary2d ! (i, j)))
        ++ (if i /= width
          then " " ++ (print2dArray' ary2d (i+1) j)
          else if j /= height
            then "\n" ++ (print2dArray' ary2d 0 (j+1))
            else ".")
    (_, (width, height)) = bounds ary2d 

disposingOfEachObjs :: StdGen -> [[(Int,Int)]] -> [Int] -> Int -> [[(Int, Int)]]
disposingOfEachObjs r0 vss ns width =
  [[(x, y)
    | x <- [0..width-1], y <- [0..height-1], (aryDim2 ! (x, y)) == i]
      | i <- [0..(length ns)-1]]
  where
    aryDim2 = disposeObjs r0 vss ns width 
    height = (sum ns) `div` width

-- 各々遺伝子とその遺伝子の使われる率
popularityOfGenes :: (Int -> String) -> Int -> [Genome] -> [(String, Int)]
popularityOfGenes nameOfInst sizeOfInsts gss =
  [(nameOfInst g, length $ concat [[x | x <- (elems gs), x == g] | gs <- gss]) | g <- [0..sizeOfInsts-1]]

-- 或る遺伝子がどれだけ使われているか
popularityOfTheGene :: [Genome] -> Int -> Int
popularityOfTheGene gss g = length $ concat [[x | x <- (elems gs), x == g] | gs <- gss] 

aCycleOfRefreshing :: InstructionSet -> GraphPaper -> GraphPaper
aCycleOfRefreshing insts world = fpow (refresh insts) (length $ toList $ world ^. ants) world

genericAlgorithm :: InstructionSet -> Int -> Int -> Int -> ([Genome] -> GraphPaper) -> [Genome] -> [Genome]
genericAlgorithm insts nRefresh nGenerate nChoise nextGeneration0 =
  fpow (choise nChoise <<< toList <<< (^. ants) <<< (fpow (aCycleOfRefreshing insts) nRefresh) <<< nextGeneration0) nGenerate
