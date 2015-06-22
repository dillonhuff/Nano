module Main(main) where

import CBackEnd
import CGen
import IndexExpression
import Matrix
import Statement

main :: IO ()
main = putStrLn $ prettyPrint 0 cOp

cOp = operationToC "matrixAdd" matOp

matOp = blockMatrixAddM (iVar "i") (iConst 8) plusABC

plusABC =
  matrixAdd a b c

a = constDblMat "A" 9 15 1 9
b = constDblMat "B" 9 15 15 1
c = constDblMat "C" 9 15 1 9

constDblMat name nr nc rs cs =
  matrix name (iConst nr) (iConst nc) (iConst rs) (iConst cs) (properties arg double)

