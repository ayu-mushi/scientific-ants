import ScientificAnts.Simulation
import ScientificAnts.InstructionSet

import System.Environment
import System.Random
import Control.Lens
import Control.Arrow
import Control.Applicative
import Control.Monad
import Data.Array
import Data.List
import Data.List.Zipper
import Options.Applicative

ancestor1 :: Genome
ancestor1 = (flip listArray) <*> (((,) 0) <<< (flip (-) 1) <<< length)
  $ asm namedInsts1 ["checkU", "checkD", "checkL", "checkR"]

mapGenomesToWorld1 :: StdGen -> [Genome] -> GraphPaper
mapGenomesToWorld1 r0 gss = toGrPp $
  disposingOfEachObjs r0
    [[(2*x, x) | x <- [0..10]],
      [(x, x) | x <- [0..10]],
      [(x, 5) | x <- [0..10]],
      [(x+1, 4*x) | x <- [0..10]],
      [(x, 5*x) | x <- [0..10]]]
      [0,50,30,20,0]
      10
  where
    toGrPp :: [[(Int, Int)]] -> GraphPaper
    toGrPp (_:(dAnt:(dSuger:(dAnteater:(dServer:_))))) = -- dは`disposing of'の略
      GraphPaper { _ants = fromList $ nextGeneration dAnt (size (instSet namedInsts1)) 0.001 r0 gss, _sugers = map Suger dSuger, _anteaters = map Anteater dAnteater, _servers = map mkServer dServer, _width = 30, _height = 30, _gen = r0 }

world1 :: StdGen -> GraphPaper
world1 = mapGenomesToWorld1 `flip` replicate 10 ancestor1

data Options = Options
  { interactive :: Bool
  , inputFile :: String
  } deriving Show

interactiveMode :: IO ()
interactiveMode = do
  putStr "SciAnts> "
  l <- getLine
  if l == "exit"
    then return ()
    else do
      putStrLn $ "command not found: " ++ l
      interactiveMode

main :: IO ()
main = interactiveMode
