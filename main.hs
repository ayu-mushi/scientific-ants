import System.Random
import ScientificAnts.Simulation
import ScientificAnts.InstructionSet
import Control.Lens
import Control.Arrow

-- methods of experiment

world1 :: StdGen -> GraphPaper
world1 r0 = toGrPp tuplizedSOEO
  where
    soeo = spacingOfEachObjects r0 [id] [50,10,10,10,10] 30 30
    take5 :: [a] -> (a, a, a, a, a)
    take5 (x:(y:(z:(w:(v:_))))) = (x, y, z, w, v)
    tuplizedSOEO = take5 soeo
    toGrPp :: ([(Int,Int)],[(Int,Int)],[(Int,Int)],[(Int,Int)],[(Int,Int)]) -> GraphPaper
    toGrPp (_, x, y, z, w) =
      (nextGeneration x (size insts1) 0.05 r0 (replicate 5 ancestor), y, z, map mkServer w)

main :: IO ()
main = print $ (fpow (refreshGraphPaper insts1) 100 (0, world1 (mkStdGen 4354353))) ^. (_2 <<< ants)
