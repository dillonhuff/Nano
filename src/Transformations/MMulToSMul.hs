module Transformations.MMulToSMul(mmulToSMul) where

import Control.Monad.State
import Data.List as L

import Matrix
import Partition
import Statement

mmulToSMul stmts =
  expandStatementsBUM mmToSM stmts

mmToSM stmt =
  case isMatrixMultiply stmt && (not $ isDotProduct stmt) && (L.all (\m -> isScalar m || isVector m) $ allOperands stmt) of
    True -> convertToSMul stmt
    False -> return [stmt]

isDotProduct stmt =
  isMatrixMultiply stmt && (isScalar $ operandWritten stmt)

convertToSMul stmt =
  case isScalar $ operandRead 1 stmt of
    True -> do
      tN <- freshRegName
      let c = operandWritten stmt
          a = operandRead 0 stmt
          b = operandRead 1 stmt
          p = properties local (dataType a) (memory)
          t = matrix tN (numRows a) (numCols a) (rowStride a) (colStride a) p in
        return [scalarMultiply t b a, matrixAdd c t c]
    False -> do
      tN <- freshRegName
      let c = operandWritten stmt
          a = operandRead 0 stmt
          b = operandRead 1 stmt
          p = properties local (dataType b) (memory)
          t = matrix tN (numRows b) (numCols b) (rowStride b) (colStride b) p in
        return [scalarMultiply t a b, matrixAdd c t c]

freshRegName :: State (String, Int, [(Matrix, Shape, Int)]) String
freshRegName = do
  (prefix, i, pts) <- get
  put $ (prefix, i + 1, pts)
  return $ prefix ++ show i
