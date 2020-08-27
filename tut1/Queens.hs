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

addConstraints :: Solver -> Integer -> [(SExpr,(Integer,Integer))] -> IO ()
addConstraints z3 n vars = do
  forM_ vars $ \(v,(i,j)) -> do
    assert z3 $ v `implies` andMany [SMT.not $ var (i,x) | x <- [1..n],x/=j]
    assert z3 $ v `implies` andMany [SMT.not $ var (x,j) | x <- [1..n],x/=i]
    assert z3 $ v `implies` andMany [SMT.not $ var (i+d,j+d) | d <- [-n..n], 0<i+d,i+d<=n, 0<j+d,j+d<=n ,d/=0]
    assert z3 $ v `implies` andMany [SMT.not $ var (i+d,j-d) | d <- [-n..n], 0<i+d,i+d<=n, 0<j-d,j-d<=n ,d/=0]

var :: (Integer,Integer) -> SExpr
var (x,y) = Atom $ "x"++show x++"_"++show y

getBoard :: Integer -> [Value] -> String
getBoard n (map (\case (Bool b) -> b) -> xs) = unlines $ go xs
  where
    go [] = []
    go xss = map (bool '-' 'X') xs : go ys
      where (xs, ys) = splitAt (fromInteger n) xss

main :: IO ()
main = do
  l <- newLogger 0
  z3 <- newSolver "z3" ["-smt2","-in"] (Just l)
  [read -> n] <- getArgs
  let board = [[(i,j) | j <- [1..n]] | i <- [1..n]]
  vars <- forM (concat board) $ \pos@(i,j) -> do
    v <- declare z3 ("x"++show i++"_"++show j) tBool
    pure (v,pos)
  addConstraints z3 n vars
  forM_ board $ \row ->
    assert z3 $ orMany $ map var row
  print =<< check z3
  solved <- map snd <$> getExprs z3 (map fst vars)
  putStrLn $ getBoard n solved
