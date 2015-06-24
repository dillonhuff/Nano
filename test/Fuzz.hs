module Fuzz(applyRandomOptimizations,
            applyTransforms, selectTransforms) where

import Control.Monad.Random
import Data.List as L

import Statement

applyTransforms :: [Statement -> [Statement]] -> [Statement] -> [Statement]
applyTransforms [] op = op
applyTransforms (t:rest) op = expandStatementsBU t $ applyTransforms rest op
  
randomBit :: (RandomGen g) => Rand g Int
randomBit = getRandomR (0, 1)

randomBits :: (RandomGen g) => Int -> Rand g [Int]
randomBits n = sequence (replicate n randomBit)

selectTransformsR :: (RandomGen g) => [a] -> Rand g [a]
selectTransformsR ts = do
  selectList <- randomBits (length ts)
  return $ L.map fst $ L.filter (\(t, b) -> b == 1) $ L.zip ts selectList

selectTransforms ts = evalRandIO (selectTransformsR ts)

applyRandomOptimizations :: [[Statement] -> [Statement]] -> [Statement] -> IO [Statement]
applyRandomOptimizations possibleOptimizations stmts = do
  toApply <- selectTransforms possibleOptimizations
  return $ applyOptimizations toApply stmts

applyOptimizations :: [[Statement] -> [Statement]] -> [Statement] -> [Statement]
applyOptimizations [] stmts = stmts
applyOptimizations (r:rest) stmts = r $ applyOptimizations rest stmts
