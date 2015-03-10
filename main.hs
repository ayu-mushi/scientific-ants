import ScientificAnts.Simulation
import ScientificAnts.InstructionSet

import System.Random
import Control.Lens
import Control.Arrow
import Data.Array
import Data.List

-- methods of experiment
ancestor1 :: Genome
ancestor1 = array (0, 6) [(0, 4), (1, 7), (2, 4), (3, 8), (4, 5), (5, 8), (6,1)]

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
        [20,30,10,20,20]
        10
    take5 :: [a] -> (a, a, a, a, a)
    take5 (x:(y:(z:(w:(v:_))))) = (x, y, z, w, v)
    tuplizedSOEO = take5 soeo
    toGrPp :: ([(Int,Int)],[(Int,Int)],[(Int,Int)],[(Int,Int)],[(Int,Int)]) -> GraphPaper
    toGrPp (_, x, y, z, w) =
      (nextGeneration x (size insts1) 0.001 r0 gss, y, z, map mkServer w, 30, 30, r0)

world1 :: StdGen -> GraphPaper
world1 r0 = mapGenomesToWorld1 r0 (replicate 10 ancestor1)

example0 :: IO ()
example0 = putStrLn $ concat $ intersperse "\n\n" $ map (concat<<<(intersperse " ")<<<unasmOfInsts1<<<elems) $ genericAlgorithm insts1 50 100 10 (mapGenomesToWorld1 (mkStdGen 324243)) (replicate 10 ancestor1)

main :: IO ()
main = print $ popularityOfGenes (size insts1) $ genericAlgorithm insts1 50 100 10 (mapGenomesToWorld1 (mkStdGen 324243)) (replicate 10 ancestor1)
