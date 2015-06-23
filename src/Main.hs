module Main(main) where

import Blocking
import CBackEnd.CodeGeneration
import CBackEnd.Syntax
import IndexExpression
import Matrix
import CBackEnd.SanityCheck
import Statement

main :: IO ()
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
