import ScientificAnts.Simulation
import ScientificAnts.InstructionSet

import System.Random
import Control.Lens
import Control.Arrow
import Data.Array

-- methods of experiment
ancestor1 :: Genome
ancestor1 = array (0, 6) [(0,0), (1,20), (2,21), (3,22), (4,23), (5,19), (6,1)]

mapGenomesToWorld1 :: StdGen -> [Genome] -> GraphPaper
mapGenomesToWorld1 r0 gss = toGrPp tuplizedSOEO
  where
    soeo =
      spacingOfEachObjects r0
        [[(x, x) | x <- [0..30]],
          [(x, 2*x) | x <- [0..30]],
          [(x, 3*x) | x <- [0..30]],
          [(x, 4*x) | x <- [0..30]],
          [(x, 5*x) | x <- [0..30]]]
        [20,20,20,20,20]
        30
    take5 :: [a] -> (a, a, a, a, a)
    take5 (x:(y:(z:(w:(v:_))))) = (x, y, z, w, v)
    tuplizedSOEO = take5 soeo
    toGrPp :: ([(Int,Int)],[(Int,Int)],[(Int,Int)],[(Int,Int)],[(Int,Int)]) -> GraphPaper
    toGrPp (_, x, y, z, w) =
      (nextGeneration x (size insts1) 0.05 r0 gss, y, z, map mkServer w)

world1 :: StdGen -> GraphPaper
world1 r0 = mapGenomesToWorld1 r0 (replicate 10 ancestor1)

main :: IO ()
main = print $ popularityOfGenes ((genericAlgorithm insts1 100 50 10 (mapGenomesToWorld1 (mkStdGen 323434))) (replicate 5 ancestor1)) (size insts1)
