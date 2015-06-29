module CostModel.Flops(flopCost) where

import Data.List as L

import Analysis.Loop
import IndexExpression
import Matrix
import Statement

flopCost :: [Statement] -> Double
flopCost op = L.foldr addFlopCost 0 op

addFlopCost stmt costSoFar =
  costSoFar + (stmtCost stmt)

stmtCost stmt =
  case isLoop stmt of
    True -> (fromIntegral $ numIterationsConst stmt) * (flopCost $ loopBody stmt)
    False -> opCost stmt

opCost stmt =
  case isMatrixMultiply stmt of
    True -> constProd [numRows $ operandWritten stmt, numCols $ operandWritten stmt, numCols $ leftOperand stmt]
    False -> constProd [numRows $ operandWritten stmt, numCols $ operandWritten stmt]

constProd [] = 1.0
constProd (x:rest) = (fromIntegral $ constVal x) * (constProd rest)
