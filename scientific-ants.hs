module ScientificAnts.Simulation where

import Control.Lens
import Data.Array
import Control.Arrow
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
type Genome = Array Int Int

type Ant = ((Int, Int), IP, Register, Stack, Hunger, Genome) -- 「蟻」
ip :: Lens' Ant IP
ip = _2
register :: Lens' Ant Register
register = _3
genome :: Lens' Ant Genome
genome = _6
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
      %~ ((+1) >>> (flip mod $ size $ ((world ^. ants) ! i) ^. genome)))

-- 時間の齣(コマ)を1つ進める
refreshGraphPaper:: InstructionSet -> (Int, GraphPaper) -> (Int, GraphPaper)
refreshGraphPaper insts (i, world) =
  ((i + 1) `mod` (size (world ^. ants)),
    (incIP i $ (insts ! ((theAnt ^. genome) ! (theAnt ^. ip))) i world))

    where
      theAnt = (world ^. ants) ! i

mutate :: Int -> (Genome, StdGen) -> (Genome, StdGen)
mutate sizeOfInsts (gm, r0) = (gm // [(i, g)], r2)
  where
    (i, r1) = randomR (0, ((size gm) - 1)) r0
    (g, r2) = randomR (0, (sizeOfInsts - 1)) r1

mkAnt :: (Int, Int) -> Genome -> Ant
mkAnt coordinates g = (coordinates, 0, (0, 0, 0, 0), [], 0, g)

-- 選択、満腹度最高よりn匹のゲノム
choise :: Int -> Array Int Ant -> [Genome]
choise n ans = map (^. genome) (folddArr selector ((size ans) - 1) ([] :: [Ant]) ans)
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
nextGeneration :: [(Int, Int)] -> Int -> Float -> StdGen -> [Genome] -> Array Int Ant
nextGeneration spacings sizeOfInsts mutationalRate r0 genomes =
  if numOfAnts == 0
    then listArray (0, 0) []
  else if (length genomes) >= numOfAnts
    then listArray (0, numOfAnts-1) $
      take numOfAnts $
        (mapWithIx (mkAnts spacings)) $ (^. _1) $
          (randmap 
            (fpow (mutate sizeOfInsts) $ round $ ((randomR (0.0, fromIntegral $ length genomes) r0) ^. _1) / mutationalRate)
            (genomes, r0))
    else 
      flatArray $
        zipWith (nextGeneration `flip` sizeOfInsts `flip` mutationalRate `flip` r0)
          (slice (length genomes) spacings)
          $ replicateWithRemainder (numOfAnts `divMod` (length genomes)) genomes 

  where
    mkAnts :: [(Int, Int)] -> Genome -> Int -> Ant
    mkAnts xys g i = mkAnt (certainsure (xys ^? ix i)) g
    numOfAnts = length spacings

    -- 確実に(たぶん)
    certainsure :: Maybe (Int, Int) -> (Int, Int)
    certainsure Nothing = error "Nothing"
    certainsure (Just x) = x

mkServer :: (Int, Int) -> Server
mkServer coordinates = (coordinates, listArray (0, 10) (replicate 11 0))

type ObjectNumber = Int
-- 0 - 「無」、1 - 「蟻」、2 - 「砂糖」、3 - 「アリジゴク」、4 - 「サーバー」

wall :: [Float] -> Float -> ObjectNumber
wall = wall' 0 
  where
    wall' :: ObjectNumber -> [Float] -> Float -> ObjectNumber
    wall' i [x] p = i
    wall' i (x:(y:xs)) p = if p < x then i else wall' (i + 1) ((x + y) : xs) p

-- ムーア近傍での最短距離は(2次元)チェビジェフ距離で測れる
chebyshevDistance :: (Int, Int) -> (Int, Int)-> Int
chebyshevDistance (p0, p1) (q0, q1) = max (abs $ p0 - q0) $ abs $ p1 - q1

-- 二次元ベクトルの列の中で最も近いやつ
distanceBetweenPtsAndPt :: (Int, Int) -> [(Int, Int)] -> Int
distanceBetweenPtsAndPt v vs = minimum $ map (chebyshevDistance v) vs

-- アイテム/エージェントの配置を決める
spacingObjects ::
  StdGen ->
  [[(Int,Int)]] -- 各々の"傾向"を表す二次元ベクトルの列の列
    -> [Int] -- 各アイテム/エージェントの個数を表す列
    -> Int -> Int -- GraphPaperの幅と高さ
    -> Array (Int,Int) ObjectNumber -- アイテム/エージェントの配置
spacingObjects r0 vss ns width height = array ((0, 0), (width-1, height-1)) $ spacingObjects' r0 ns 0 0 [] 
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
        distances = map (distanceBetweenPtsAndPt (i, j)) vss
        weight :: [Float]
        weight = map ((/ (fromIntegral $ sum distances)) <<< fromIntegral) distances
        weightedNs = zipWith (*) (map fromIntegral ns) weight
        (x, r1) = randomR (0.0, sum weightedNs) r0
        obj = wall weightedNs x

print2dArray :: (Show a) => Array (Int, Int) a -> String
print2dArray ary2d = print2dArray' ary2d 0 0
  where
    print2dArray' :: (Show a) => Array (Int, Int) a -> Int -> Int -> String
    print2dArray' ary2d i j = 
      (show (ary2d ! (j, i)))
        ++ (if i /= width
          then " " ++ (print2dArray' ary2d (i+1) j)
          else if j /= height
            then "\n" ++ (print2dArray' ary2d 0 (j+1))
            else ".")
    (_, (width, height)) = bounds ary2d 

spacingOfEachObjects :: StdGen -> [[(Int,Int)]] -> [Int] -> Int -> Int -> [[(Int, Int)]]
spacingOfEachObjects r0 vss ns width height =
  [[(x, y)
    | x <- [0..width-1], y <- [0..height-1], (aryDim2 ! (x, y)) == i]
      | i <- [0..(length ns)-1]]
  where
    aryDim2 = spacingObjects r0 vss ns width height 

-- 各々遺伝子とその遺伝子の使われる率
popularityOfGenes :: [Genome] -> Int -> [(Int, Int)]
popularityOfGenes gss sizeOfInsts =
  [(g, length $ concat [[x | x <- (elems gs), x == g] | gs <- gss]) | g <- [0..sizeOfInsts-1]]

-- 或る遺伝子がどれだけ使われているか
popularityOfTheGene :: [Genome] -> Int -> Int
popularityOfTheGene gss g = length $ concat [[x | x <- (elems gs), x == g] | gs <- gss] 
