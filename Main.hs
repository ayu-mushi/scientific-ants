{-# LANGUAGE TemplateHaskell #-}

import ScientificAnts.Engine
import ScientificAnts.Instruct

import Control.DeepSeq (rnf)
import System.Random (StdGen, mkStdGen)
import qualified System.IO as IO
import Control.Lens hiding (argument)
import Control.Arrow
import Control.Applicative
import Control.Monad
import Control.Monad.State
import Data.Array (listArray)
import Data.List
import Data.List.Zipper (Zipper, insert, fromList)
import Data.Monoid (mconcat)
import Options.Applicative

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
        info (refreshCmd <$> argument str idm) idm
    , command "create" $
        info (createCmd <$> argument str idm) idm
    ]

interactiveMode :: (Zipper GraphPaper) -> IO ()
interactiveMode worlds = do
  putStr "SciAnts> "
  l <- getLine
  when (l /= "exit") $ do
    (exe, worlds') <- (execParser $ info opts idm) >>= ((runState `flip` worlds) >>> return)
    exe
    interactiveMode worlds'

modifyFile :: IO.FilePath -> (String -> String) -> IO ()
modifyFile filename f = do
  h <- IO.openFile filename IO.ReadMode
  contents <- IO.hGetContents h
  return $! rnf contents
  IO.hClose h
  IO.writeFile filename $ f contents

main :: IO ()
main = do 
  h <- IO.openFile "world1.scian" IO.ReadMode
  worlds <- IO.hGetContents h
  return $! rnf worlds
  IO.hClose h
  (exe, worlds') <- (execParser $ info opts idm) >>= ((runState `flip` ((read :: String -> Zipper GraphPaper) $ worlds)) >>> return)
  IO.writeFile "world1.scian" $ show worlds'
  exe
