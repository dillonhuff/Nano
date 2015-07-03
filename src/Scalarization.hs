module Scalarization(scalarize) where

import Control.Monad.State
import Data.List as L

import Analysis.Matrix
import Analysis.Statement
import IndexExpression
import Matrix
import Statement

scalarize :: Int -> String -> [Statement] -> [Statement]
scalarize u uniqueVarPrefix stmts =
  evalState (expandStatementsBUM (tryToScalarize u) stmts) (uniqueVarPrefix, 0)

tryToScalarize :: Int -> Statement -> State (String, Int) [Statement]
tryToScalarize u stmt =
  case opcode stmt of
{-    BRDC -> case (isRegisterizeable u $ operandWritten stmt) &&
                 (isScalar $ operandRead 0 stmt) of
              True -> registerizeBroadcast stmt
              False -> return [stmt]-}
    EMUL ->
      case isScalarOp u stmt of
        True -> registerizeEMUL u stmt
        False -> return [stmt]
    _ ->
      case not (isLoop stmt) && isScalarOp u stmt of
        True -> scalarizeStmt stmt
        False -> return [stmt]

scalarizeStmt :: Statement -> State (String, Int) [Statement]
scalarizeStmt stmt =
  case isMatrixAdd stmt of
    True -> scalarizeMAdd stmt
    False -> case isMatrixTranspose stmt of
      True -> scalarizeTrans stmt
      False -> case isMatrixMultiply stmt of
        True -> scalarizeMMul stmt
        False -> case isScalarMultiply stmt of
          True -> scalarizeSMul stmt
          False -> case isMatrixSet stmt of
            True -> scalarizeTrans stmt
            False -> error $ "scalarizeStmt: Unsupported operation " ++ show stmt

freshRegName :: State (String, Int) String
freshRegName = do
  (prefix, i) <- get
  put $ (prefix, i + 1)
  return $ prefix ++ show i

{-registerizeBroadcast stmt =
  let a = operandWritten stmt
      b = operandRead 0 stmt in
  do
    r1Name <- freshRegName
    let r1 = duplicateInRegister r1Name b in
      return [matrixSet r1 b, broadcast a r1]-}
  
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
      return [matrixSet r1 a, matrixSet r2 b, matrixAdd r3 r1 r2, matrixSet c r3]

registerizeEMUL u stmt = 
  let c = operandWritten stmt
      a = operandRead 0 stmt
      b = operandRead 1 stmt in
  do
    r1Name <- freshRegName
    r2Name <- freshRegName
    r3Name <- freshRegName
    let r1 = duplicateInRegister r1Name a
        r2 = duplicateInRegister r2Name b
        r3 = duplicateInRegister r3Name c in
      return [matrixSet r1 a, matrixSet r2 b, elemWiseMultiply r3 r1 r2, matrixSet c r3]

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
      return [matrixSet r1 alpha, matrixSet r2 b, scalarMultiply r3 r1 r2, matrixSet c r3]

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

scalarizeTrans stmt =
  let a = operandWritten stmt
      b = rightOperand stmt in
  do
    r1Name <- freshRegName
    let r1 = duplicateInRegister r1Name b in
      return [matrixSet r1 b, matrixSet a r1]
  
duplicateInRegister rName a =
  setName rName $ setRegister a
