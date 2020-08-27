{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
module Main where
  
import Control.Monad
import Text.Read
import System.Environment
import System.IO
import SimpleSMT hiding (concat)
import qualified SimpleSMT as SMT
import Data.List

loadSudoku :: Solver -> Handle -> IO [[SExpr]]
loadSudoku z3 h = goLine 0
  where
    goLine i = do
      eof <- hIsEOF h
      if eof then return []
      else do
        xs' <- map readMaybe . words <$> hGetLine h
        xs <- forM (zip xs' [0..]) $ \case
          (Just n,_) -> pure $ int n
          (Nothing,j) -> do
             v <- declare z3 ("x"++show i++show j) tInt
             assert z3 $ v `gt` int 0
             assert z3 $ v `lt` int 10
             pure v
        xss <- goLine (i+1)
        pure (xs:xss)

addConstraints :: Solver -> [[SExpr]] -> IO ()
addConstraints z3 sudoku = do
   mapM_ (assert z3 . distinct) $ sudoku
   mapM_ (assert z3 . distinct) $ transpose sudoku
   mapM_ (assert z3 . distinct) $ boxes sudoku

boxes :: [[a]] -> [[a]]
boxes xss =
  [[xss !! (u + i) !! (v + j)
      | i <- [0..2]
      , j <- [0..2]]
   | u <- [0,3,6]
   , v <- [0,3,6]]

solvedToSudoku :: [Value] -> [[Integer]]
solvedToSudoku (map (\case (Int i) -> i) -> xs) = go xs
  where
    go [] = []
    go xss = xs : go ys
      where (xs, ys) = splitAt 9 xss

main :: IO ()
main = do
  l <- newLogger 0
  z3 <- newSolver "z3" ["-smt2","-in"] (Just l)
  [file] <- getArgs
  sudoku <- withFile file ReadMode $ loadSudoku z3
  addConstraints z3 sudoku
  print =<< check z3
  solved <- solvedToSudoku . map snd <$> getExprs z3 (concat sudoku)
  putStrLn $ unlines $ map (unwords . map show) $ solved
