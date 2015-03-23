import ScientificAnts.Simulation
import ScientificAnts.InstructionSet

import System.Random
import Control.Lens
import Control.Arrow
import Data.Array
import Data.List

ancestor1 :: Genome
ancestor1 = (listArray & uncurry) $
  (((,) 0) <<< (flip (-) 1) <<< length) &&& id $
    asmOfInsts1 $
      ["checkU", "checkD", "checkL", "checkR"]

mapGenomesToWorld1 :: StdGen -> [Genome] -> GraphPaper
mapGenomesToWorld1 r0 gss = toGrPp tuplizedSOEO
  where
    soeo =
      spacingOfEachObjects r0
        [[(2*x, x) | x <- [0..10]],
          [(x, x) | x <- [0..10]],
          [(x, 5) | x <- [0..10]],
          [(x+1, 4*x) | x <- [0..10]],
          [(x, 5*x) | x <- [0..10]]]
        [0,50,30,20,0]
        10
    take5 :: [a] -> (a, a, a, a, a)
    take5 (x:(y:(z:(w:(v:_))))) = (x, y, z, w, v)
    tuplizedSOEO = take5 soeo
    toGrPp :: ([(Int,Int)],[(Int,Int)],[(Int,Int)],[(Int,Int)],[(Int,Int)]) -> GraphPaper
    toGrPp (_, x, y, z, w) =
      (nextGeneration x (size insts1) 0.001 r0 gss, y, z, map mkServer w, 30, 30, r0)

world1 :: StdGen -> GraphPaper
world1 r0 = mapGenomesToWorld1 r0 (replicate 10 ancestor1)

justGenomesExample0 :: IO ()
justGenomesExample0 = putStrLn $ concat $ intersperse "\n\n" $ map (concat<<<(intersperse " ")<<<unasmOfInsts1<<<elems) $ genericAlgorithm insts1 50 100 10 (mapGenomesToWorld1 (mkStdGen 324243)) (replicate 10 ancestor1)

-- methods of experiment
-- Axelrod「An Evolutionary Approach to Norm」風に、報酬の有無によって協調(情報提供)率が上がるかを調べる
experiment1 :: IO ()
experiment1 = putStrLn $
  (show $ map (\r0 -> (popularityOfTheGene
    (genericAlgorithm
      (insts1
        // [(numberOfInst1 "rewardU", flip const),
          (numberOfInst1 "rewardD", flip const),
          (numberOfInst1 "rewardL", flip const),
          (numberOfInst1 "rewardR", flip const)])
      50 10 10 (mapGenomesToWorld1 (mkStdGen r0)) (replicate 10 ancestor1)))
      $ numberOfInst1 "mentionU") [143243..143253])
  ++ "\n" ++
  (show $ map (\r0 -> popularityOfTheGene
    (genericAlgorithm
      insts1
      50 10 10 (mapGenomesToWorld1 (mkStdGen r0)) (replicate 10 ancestor1))
      $ numberOfInst1 "mentionU") [45823..45833])

main :: IO ()
main = experiment1
