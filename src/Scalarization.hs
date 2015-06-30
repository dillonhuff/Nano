module Scalarization(scalarize) where

import Control.Monad.State
import Data.List as L

import Matrix
import Statement

scalarize :: String -> [Statement] -> [Statement]
scalarize uniqueVarPrefix stmts =
  evalState (expandStatementsBUM tryToScalarize stmts) (uniqueVarPrefix, 0)

tryToScalarize :: Statement -> State (String, Int) [Statement]
tryToScalarize stmt =
  case not (isLoop stmt) && isScalarOp stmt of
    True -> scalarizeStmt stmt
    False -> return [stmt]

isScalarOp :: Statement -> Bool
isScalarOp stmt = L.all isScalar $ allOperands stmt

scalarizeStmt :: Statement -> State (String, Int) [Statement]
scalarizeStmt stmt =
  case isMatrixAdd stmt of
    True -> scalarizeMAdd stmt
    False -> case isMatrixTranspose stmt of
      True -> error "scalarize matrix transpose"
      False -> case isMatrixMultiply stmt of
        True -> scalarizeMMul stmt
        False -> case isScalarMultiply stmt of
          True -> scalarizeSMul stmt
          False -> error $ "scalarizeStmt: Unsupported operation " ++ show stmt

freshRegName :: State (String, Int) String
freshRegName = do
  (prefix, i) <- get
  put $ (prefix, i + 1)
  return $ prefix ++ show i

scalarizeMAdd stmt =
  let c = operandWritten stmt
      a = leftOperand stmt
      b = rightOperand stmt in
  do
    r1Name <- freshRegName
    r2Name <- freshRegName
    r3Name <- freshRegName
    let r1 = duplicateInRegister r1Name a
        r2 = duplicateInRegister r2Name b
        r3 = duplicateInRegister r3Name c in
      return [matrixSet r1 a, matrixSet r2 b, matrixSet r3 c, matrixAdd r3 r1 r2, matrixSet c r3]

scalarizeSMul stmt =
  let c = operandWritten stmt
      alpha = leftOperand stmt
      b = rightOperand stmt in
  do
    r1Name <- freshRegName
    r2Name <- freshRegName
    r3Name <- freshRegName
    let r1 = duplicateInRegister r1Name alpha
        r2 = duplicateInRegister r2Name b
        r3 = duplicateInRegister r3Name c in
      return [matrixSet r1 alpha, matrixSet r2 b, matrixSet r3 c, scalarMultiply r3 r1 r2, matrixSet c r3]

scalarizeMMul stmt =
  let c = operandWritten stmt
      a = leftOperand stmt
      b = rightOperand stmt in
  do
    r1Name <- freshRegName
    r2Name <- freshRegName
    r3Name <- freshRegName
    let r1 = duplicateInRegister r1Name a
        r2 = duplicateInRegister r2Name b
        r3 = duplicateInRegister r3Name c in
      return [matrixSet r1 a, matrixSet r2 b, matrixSet r3 c, matrixMultiply r3 r1 r2, matrixSet c r3]

duplicateInRegister rName a =
  setName rName $ setRegister a
