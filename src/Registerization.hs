module Registerization(registerize) where

import Control.Monad.State
import Data.List as L

import Analysis.Matrix
import Analysis.Statement
import IndexExpression
import Matrix
import Statement

registerize :: Int -> String -> [Statement] -> [Statement]
registerize u uniqueVarPrefix stmts =
  evalState (expandStatementsBUM (tryToRegisterize u) stmts) (uniqueVarPrefix, 0)

tryToRegisterize :: Int -> Statement -> State (String, Int) [Statement]
tryToRegisterize u stmt =
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
        True -> registerizeStmt stmt
        False -> return [stmt]

registerizeStmt :: Statement -> State (String, Int) [Statement]
registerizeStmt stmt =
  case isMatrixAdd stmt of
    True -> registerizeMAdd stmt
    False -> case isMatrixTranspose stmt of
      True -> registerizeTrans stmt
      False -> case isMatrixMultiply stmt of
        True -> registerizeMMul stmt
        False -> case isScalarMultiply stmt of
          True -> registerizeSMul stmt
          False -> case isMatrixSet stmt of
            True -> registerizeTrans stmt
            False -> error $ "registerizeStmt: Unsupported operation " ++ show stmt

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
  
registerizeMAdd stmt =
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

registerizeSMul stmt =
  let c = operandWritten stmt
      alpha = operandRead 0 stmt
      b = operandRead 1 stmt in
  do
    r1Name <- freshRegName
    r2Name <- freshRegName
    r3Name <- freshRegName
    let r1 = duplicateInRegister r1Name alpha
        r2 = duplicateInRegister r2Name b
        r3 = duplicateInRegister r3Name c in
      return [matrixSet r1 alpha, matrixSet r2 b, scalarMultiply r3 r1 r2, matrixSet c r3]

registerizeMMul stmt =
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
      return [matrixSet r1 a, matrixSet r2 b, matrixSet r3 c, matrixMultiply r3 r1 r2, matrixSet c r3]

registerizeTrans stmt =
  let a = operandWritten stmt
      b = operandRead 0 stmt in
  do
    r1Name <- freshRegName
    let r1 = duplicateInRegister r1Name b in
      return [matrixSet r1 b, matrixSet a r1]
  
duplicateInRegister rName a =
  setName rName $ setRegister a
