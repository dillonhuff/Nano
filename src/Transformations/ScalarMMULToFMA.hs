module Transformations.ScalarMMULToFMA(scalarMMULToFMA) where

import Control.Monad.State
import Data.List as L

import Analysis.Matrix
import Core.IndexExpression
import Core.Matrix
import Core.Statement
import Utils

scalarMMULToFMA uniqueVarPrefix stmts =
  evalState (expandStatementsBUM mmulToFMA stmts) (uniqueVarPrefix, 0)

mmulToFMA stmt =
  case isScalarMMUL stmt of
    True -> toFMA stmt
    False -> return [stmt]

toFMA stmt =
  do
    t1N <- freshRegName
    t2N <- freshRegName
    t3N <- freshRegName
    let c = operandWritten stmt
        a = operandRead 0 stmt
        b = operandRead 1 stmt
        t1 = duplicateInTemp t1N a
        t2 = duplicateInTemp t2N b
        t3 = duplicateInTemp t3N c in
      return [fusedMultiplyAdd c a b]

freshRegName :: State (String, Int) String
freshRegName = do
  (prefix, i) <- get
  put $ (prefix, i + 1)
  return $ prefix ++ show i

isScalarMMUL stmt =
  opcode stmt == MMUL &&
  (L.all isScalar $ allOperands stmt)
