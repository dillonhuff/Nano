module Main(main) where

import Blocking
import CBackEnd.CodeGeneration
import CBackEnd.Syntax
import IndexExpression
import Matrix
import CBackEnd.SanityCheck
import Statement

{-main :: IO ()
main = do
  scResStr <- runSanityCheck "mainMAddTest" maddOp maddRefinedOp args
  putStrLn $ scResStr
  where
    (maddOp, args) = operationToC "matrixAdd" [plusABC]
    (maddRefinedOp, _) = operationToC "optimizedMatrixAdd" optimizedPlusABC

optimizedPlusABC = blockMatrixAddM (iVar "i") (iConst 8) plusABC

plusABC =
  matrixAdd a b c

a = constDblMat "A" 9 15 1 9
b = constDblMat "B" 9 15 15 1
c = constDblMat "C" 9 15 1 9

constDblMat name nr nc rs cs =
  matrix name (iConst nr) (iConst nc) (iConst rs) (iConst cs) (properties arg double)

-}

main :: IO ()
main = do
  res <- testBlocking blockScalarMultiplyM (iConst 2, smulCAlphaA)
  putStrLn $ show res

testBlocking blkFunc (blkFactor, stmt) = do
  putStrLn $ show [stmt]
  putStrLn $ show $ argInfoList [stmt]

a = constDblMat "A" 9 9 1 9
b = constDblMat "B" 9 9 1 9
c = constDblMat "C" 9 9 1 9

alpha = constDblMat "alpha" 1 1 1 1

maddCBA = matrixAdd c b a
smulCAlphaA = scalarMultiply c alpha a

constDblMat name nr nc rs cs =
  matrix name (iConst nr) (iConst nc) (iConst rs) (iConst cs) (properties arg double)
