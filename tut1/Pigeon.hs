{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
module Main where
  
import Control.Monad
import Text.Read
import System.Environment
import System.IO
import SimpleSMT hiding (concat, bool)
import qualified SimpleSMT as SMT
import Data.List
import Data.Bool

type Pigeon = Integer
type Hole = Integer

addConstraints :: Solver -> Integer -> [[SExpr]] -> IO ()
addConstraints z3 n pigeons = do
  forM_ pigeons $ \pigeon -> assert z3 $ orMany pigeon -- Each pigeon is in at least 1 hole

  assert z3 $ SMT.not $ orMany $ [ orMany [SMT.and p1 p2 -- There are two distinct pigeons in that hole
                                          | (p1:ps) <- tails hole
                                          , p2      <- ps
                                          ]
                                 | hole <- transpose pigeons] -- For some hole

pigeon :: Pigeon -> Hole -> SExpr
pigeon x y = Atom $ "p"++show x++"_"++show y

main :: IO ()
main = do
  l <- newLogger 0
  z3 <- newSolver "z3" ["-smt2","-in"] (Just l)
  [read -> n] <- getArgs
  -- pigeons :: (n+1)*n
  pigeons <- forM [[(i,j) | j <- [1..n]] | i <- [1..n+1]] $ mapM $ \(i,j) ->
    declare z3 ("p"++show i++"_"++show j) tBool
  addConstraints z3 n pigeons
  print =<< check z3
