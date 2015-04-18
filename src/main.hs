{-# LANGUAGE TemplateHaskell #-}
import ScientificAnts.Simulation
import ScientificAnts.InstructionSet

import System.Random
import System.IO
import Control.Lens
import Control.Arrow
import Control.Applicative
import Control.Monad
import Control.Monad.State
import Data.Array
import Data.List
import Data.List.Zipper
import Options.Applicative
import Data.Monoid

ancestor1 :: Genome
ancestor1 = (flip listArray) <*> (((,) 0) <<< (flip (-) 1) <<< length)
  $ asm namedInsts1 ["checkU", "checkD", "checkL", "checkR"]

mapGenomesToWorld1 :: StdGen -> [Genome] -> GraphPaper
mapGenomesToWorld1 r0 gss = create $
  disposingOfEachObjs r0
    [[(2*x, x) | x <- [0..10]],
      [(x, x) | x <- [0..10]],
      [(x, 5) | x <- [0..10]],
      [(x+1, 4*x) | x <- [0..10]],
      [(x, 5*x) | x <- [0..10]]]
      ns
      w
  where
    create :: [[(Int, Int)]] -> GraphPaper
    create (_:(dAnt:(dSuger:(dAnteater:(dServer:_))))) = -- dは`disposing of'の略
      GraphPaper { _ants = fromList $ nextGeneration dAnt (size (instSet namedInsts1)) 0.001 r0 gss, _sugers = map Suger dSuger, _anteaters = map Anteater dAnteater, _servers = map mkServer dServer, _width = w, _height = (sum ns) `div` w, _gen = r0 }
    w = 10
    ns = [0,50,30,20,0]

world1 :: StdGen -> GraphPaper
world1 = mapGenomesToWorld1 `flip` replicate 10 ancestor1

refreshCmd :: String -> State (Zipper GraphPaper) (IO ())
refreshCmd = state <<< (((,) $ return ()) <<<) <<< (((%~) focus) <<< (fpow $ refresh $ instSet namedInsts1)) <<< (read :: String -> Int)

createCmd :: String -> State (Zipper GraphPaper) (IO ())
createCmd = state <<< (((,) $ return ()) <<<) <<< Data.List.Zipper.insert <<< world1 <<< read

opts :: Parser (State (Zipper GraphPaper) (IO ()))
opts =
  subparser $ mconcat
    [ command "refresh" $
        info (refreshCmd <$> Options.Applicative.argument str idm) idm
    , command "create" $
        info (createCmd <$> Options.Applicative.argument str idm) idm
    ]

interactiveMode :: (Zipper GraphPaper) -> IO ()
interactiveMode worlds = do
  putStr "SciAnts> "
  l <- getLine
  when (l /= "exit") $ do
    (exe, worlds') <- (execParser $ info opts idm) >>= ((runState `flip` worlds) >>> return)
    exe
    interactiveMode worlds'

readWorlds :: FilePath -> IO (Zipper GraphPaper)
readWorlds filename = do
  handle <- openFile filename ReadMode
  contents <- hGetContents handle
  hClose handle
  return (read contents :: Zipper GraphPaper)

writeWorlds :: (Zipper GraphPaper) -> FilePath -> IO ()
writeWorlds worlds filename = do
  handle <- openFile filename WriteMode
  hPutStr handle $ show worlds
  hClose handle

main :: IO ()
main = do
  (exe, worlds') <- (execParser $ info opts idm) >>= ((runState `flip` (fromList [world1 $ mkStdGen 114514])) >>> return)
  print $ popularityOfGenes (nameOfInst $ nameSet namedInsts1) (size $ instSet namedInsts1) $ map (^. genome) $ toList $ worlds' ^. focus ^. ants
  exe
