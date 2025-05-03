{-# LANGUAGE ScopedTypeVariables #-}
module Polarity (polarity) where

import Control.Monad
import Control.Monad.ST
import Data.Array.ST
import qualified Data.Array.ST as STArr  -- for freeze
import Data.Array (Array, (!))
import qualified Data.Array as A
import Data.Maybe (isJust, fromJust)
import Data.List (sortBy)
import Data.Ord (comparing)

-- A domino is a pair of positions.
type Domino = ((Int,Int), (Int,Int))

-- | Main function. Given an orientation board and constraints (left, right, top, bottom),
-- produce a solved board (each cell is either 'X', '+' or '-').
polarity :: [String] -> ([Int], [Int], [Int], [Int]) -> [String]
polarity board (leftCons, rightCons, topCons, bottomCons) = runST $ do
  let numRows = length board
      numCols = length (head board)
  grid <- newArray ((0,0), (numRows-1, numCols-1)) ' ' 
           :: ST s (STArray s (Int,Int) Char)
  rowPlus     <- newArray (0, numRows-1) 0 :: ST s (STArray s Int Int)
  rowMinus    <- newArray (0, numRows-1) 0 :: ST s (STArray s Int Int)
  rowAssigned <- newArray (0, numRows-1) 0 :: ST s (STArray s Int Int)
  colPlus     <- newArray (0, numCols-1) 0 :: ST s (STArray s Int Int)
  colMinus    <- newArray (0, numCols-1) 0 :: ST s (STArray s Int Int)
  colAssigned <- newArray (0, numCols-1) 0 :: ST s (STArray s Int Int)
  
  let dominoes0 = computeDominoes board
      dominoes   = orderDominoes dominoes0 numRows numCols
  
  solved <- backtrack 0 grid rowPlus rowMinus rowAssigned colPlus colMinus colAssigned
                      dominoes leftCons rightCons topCons bottomCons numRows numCols
  if solved then do
      frozen <- STArr.freeze grid  -- let the compiler infer type: Array (Int,Int) Char
      let rows = [ [ frozen ! (r,c) | c <- [0..numCols-1] ]
                   | r <- [0..numRows-1] ]
      return rows
    else
      error "No solution found"



backtrack :: Int
          -> STArray s (Int,Int) Char
          -> STArray s Int Int  -- rowPlus
          -> STArray s Int Int  -- rowMinus
          -> STArray s Int Int  -- rowAssigned
          -> STArray s Int Int  -- colPlus
          -> STArray s Int Int  -- colMinus
          -> STArray s Int Int  -- colAssigned
          -> [Domino]
          -> [Int] -> [Int] -> [Int] -> [Int]  
          -> Int          
          -> Int          
          -> ST s Bool
backtrack idx grid rowPlus rowMinus rowAssigned colPlus colMinus colAssigned dominoes leftCons rightCons topCons bottomCons numRows numCols
  | idx == length dominoes = finalCheck grid leftCons rightCons topCons bottomCons numRows numCols
  | otherwise = do
      let ((r1,c1),(r2,c2)) = dominoes !! idx
      tryAssign ((r1,c1),(r2,c2)) [('X','X'), ('+','-'), ('-','+')]
  where
    
    tryAssign domino [] = return False
    tryAssign ((r1,c1),(r2,c2)) ((a,b):rest) = do
      cell1 <- readArray grid (r1,c1)
      cell2 <- readArray grid (r2,c2)
      if cell1 /= ' ' || cell2 /= ' '
         then error "Cell already assigned"
         else do
           
           oldR1Plus     <- readArray rowPlus r1
           oldR1Minus    <- readArray rowMinus r1
           oldR1Assigned <- readArray rowAssigned r1
           oldC1Plus     <- readArray colPlus c1
           oldC1Minus    <- readArray colMinus c1
           oldC1Assigned <- readArray colAssigned c1

           oldR2Plus     <- readArray rowPlus r2
           oldR2Minus    <- readArray rowMinus r2
           oldR2Assigned <- readArray rowAssigned r2
           oldC2Plus     <- readArray colPlus c2
           oldC2Minus    <- readArray colMinus c2
           oldC2Assigned <- readArray colAssigned c2

         
           writeArray grid (r1,c1) a
           writeArray grid (r2,c2) b
           updateMutable r1 c1 a rowPlus rowMinus rowAssigned colPlus colMinus colAssigned
           updateMutable r2 c2 b rowPlus rowMinus rowAssigned colPlus colMinus colAssigned

           valid1 <- checkLocal grid (r1,c1) a numRows numCols
           valid2 <- checkLocal grid (r2,c2) b numRows numCols
           feas   <- if valid1 && valid2 
                       then checkFeasible rowPlus rowMinus rowAssigned colPlus colMinus colAssigned
                              leftCons rightCons topCons bottomCons numRows numCols
                       else return False

           if not (valid1 && valid2 && feas)
             then do
               rollback r1 (r1,c1) grid rowPlus rowMinus rowAssigned oldR1Plus oldR1Minus oldR1Assigned
               rollbackC c1 (r1,c1) grid colPlus colMinus colAssigned oldC1Plus oldC1Minus oldC1Assigned
               rollback r2 (r2,c2) grid rowPlus rowMinus rowAssigned oldR2Plus oldR2Minus oldR2Assigned
               rollbackC c2 (r2,c2) grid colPlus colMinus colAssigned oldC2Plus oldC2Minus oldC2Assigned
               writeArray grid (r1,c1) ' '
               writeArray grid (r2,c2) ' '
               tryAssign ((r1,c1),(r2,c2)) rest
             else do
               succ <- backtrack (idx+1) grid rowPlus rowMinus rowAssigned colPlus colMinus colAssigned
                         dominoes leftCons rightCons topCons bottomCons numRows numCols
               if succ then return True else do
                 rollback r1 (r1,c1) grid rowPlus rowMinus rowAssigned oldR1Plus oldR1Minus oldR1Assigned
                 rollbackC c1 (r1,c1) grid colPlus colMinus colAssigned oldC1Plus oldC1Minus oldC1Assigned
                 rollback r2 (r2,c2) grid rowPlus rowMinus rowAssigned oldR2Plus oldR2Minus oldR2Assigned
                 rollbackC c2 (r2,c2) grid colPlus colMinus colAssigned oldC2Plus oldC2Minus oldC2Assigned
                 writeArray grid (r1,c1) ' '
                 writeArray grid (r2,c2) ' '
                 tryAssign ((r1,c1),(r2,c2)) rest



updateMutable :: Int -> Int -> Char -> STArray s Int Int -> STArray s Int Int -> STArray s Int Int ->
                 STArray s Int Int -> STArray s Int Int -> STArray s Int Int -> ST s ()
updateMutable r c v rowPlus rowMinus rowAssigned colPlus colMinus colAssigned = do
  modifyArray rowAssigned r (+1)
  modifyArray colAssigned c (+1)
  when (v == '+') $ do
    modifyArray rowPlus r (+1)
    modifyArray colPlus c (+1)
  when (v == '-') $ do
    modifyArray rowMinus r (+1)
    modifyArray colMinus c (+1)

modifyArray :: STArray s Int Int -> Int -> (Int -> Int) -> ST s ()
modifyArray arr i f = do
  x <- readArray arr i
  writeArray arr i (f x)

rollback :: Int -> (Int,Int) -> STArray s (Int,Int) Char -> STArray s Int Int -> STArray s Int Int -> STArray s Int Int -> Int -> Int -> Int -> ST s ()
rollback r _ _ rowPlus rowMinus rowAssigned oldP oldM oldA = do
  writeArray rowPlus r oldP
  writeArray rowMinus r oldM
  writeArray rowAssigned r oldA

rollbackC :: Int -> (Int,Int) -> STArray s (Int,Int) Char -> STArray s Int Int -> STArray s Int Int -> STArray s Int Int -> Int -> Int -> Int -> ST s ()
rollbackC c _ _ colPlus colMinus colAssigned oldP oldM oldA = do
  writeArray colPlus c oldP
  writeArray colMinus c oldM
  writeArray colAssigned c oldA


checkLocal :: STArray s (Int,Int) Char -> (Int,Int) -> Char -> Int -> Int -> ST s Bool
checkLocal grid (r,c) v numRows numCols
  | v == 'X'  = return True
  | otherwise = do
      let nbrs = [(r-1,c), (r+1,c), (r,c-1), (r,c+1)]
      results <- forM nbrs $ \(nr,nc) ->
         if nr < 0 || nr >= numRows || nc < 0 || nc >= numCols
           then return True
           else do neigh <- readArray grid (nr,nc)
                   return (neigh == ' ' || neigh == 'X' || neigh /= v)
      return (and results)

checkFeasible :: STArray s Int Int -> STArray s Int Int -> STArray s Int Int ->
                 STArray s Int Int -> STArray s Int Int -> STArray s Int Int ->
                 [Int] -> [Int] -> [Int] -> [Int] -> Int -> Int -> ST s Bool
checkFeasible rowPlus rowMinus rowAssigned colPlus colMinus colAssigned leftCons rightCons topCons bottomCons numRows numCols = do
  rowOK <- forM [0..numRows-1] $ \r -> do
      rp  <- readArray rowPlus r
      rm  <- readArray rowMinus r
      ass <- readArray rowAssigned r
      let remaining = numCols - ass
          reqP = leftCons !! r
          reqM = rightCons !! r
      return $ (reqP == -1 || (rp <= reqP && rp + remaining >= reqP)) &&
               (reqM == -1 || (rm <= reqM && rm + remaining >= reqM))
  colOK <- forM [0..numCols-1] $ \c -> do
      cp  <- readArray colPlus c
      cm  <- readArray colMinus c
      ass <- readArray colAssigned c
      let remaining = numRows - ass
          reqP = topCons !! c
          reqM = bottomCons !! c
      return $ (reqP == -1 || (cp <= reqP && cp + remaining >= reqP)) &&
               (reqM == -1 || (cm <= reqM && cm + remaining >= reqM))
  return (and rowOK && and colOK)

finalCheck :: STArray s (Int,Int) Char -> [Int] -> [Int] -> [Int] -> [Int] -> Int -> Int -> ST s Bool
finalCheck grid leftCons rightCons topCons bottomCons numRows numCols = do
  frozen <- STArr.freeze grid  
  let rows = [ [ frozen ! (r,c) | c <- [0..numCols-1] ] | r <- [0..numRows-1] ]
      cols = transpose rows
      plusRows  = map (length . filter (== '+')) rows
      minusRows = map (length . filter (== '-')) rows
      plusCols  = map (length . filter (== '+')) cols
      minusCols = map (length . filter (== '-')) cols
      okRows  = and $ zipWith (\p req -> req == -1 || p == req) plusRows leftCons
      okRows' = and $ zipWith (\m req -> req == -1 || m == req) minusRows rightCons
      okCols  = and $ zipWith (\p req -> req == -1 || p == req) plusCols topCons
      okCols' = and $ zipWith (\m req -> req == -1 || m == req) minusCols bottomCons
  return (okRows && okRows' && okCols && okCols')



computeDominoes :: [String] -> [Domino]
computeDominoes board =
  let numRows = length board
      numCols = length (head board)
      positions = [(r,c) | r <- [0..numRows-1], c <- [0..numCols-1]]
      dominoFor (r,c) =
         case board !! r !! c of
           'T' -> if r+1 < numRows then Just ((r,c), (r+1,c)) else Nothing
           'L' -> if c+1 < numCols then Just ((r,c), (r,c+1)) else Nothing
           _   -> Nothing
  in [ d | pos <- positions, let md = dominoFor pos, isJust md, let d = fromJust md ]

orderDominoes :: [Domino] -> Int -> Int -> [Domino]
orderDominoes ds numRows numCols =
  sortBy (comparing (negate . dominoDegree numRows numCols)) ds

dominoDegree :: Int -> Int -> Domino -> Int
dominoDegree numRows numCols ((r1,c1),(r2,c2)) =
  let deg (r,c) = length $ filter (\(nr,nc) -> nr >= 0 && nr < numRows && nc >= 0 && nc < numCols)
                         [(r-1,c), (r+1,c), (r,c-1), (r,c+1)]
  in deg (r1,c1) + deg (r2,c2)

transpose :: [[a]] -> [[a]]
transpose ([]:_) = []
transpose xs     = map head xs : transpose (map tail xs)
