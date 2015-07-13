module Transformations.Registerization(registerize,
                       registerizeBelow) where

import Control.Monad.State
import Data.List as L

import Analysis.Matrix
import Analysis.Statement
import Core.IndexExpression
import Core.Matrix
import Core.Statement
import Utils

registerize :: Int -> String -> [Statement] -> [Statement]
registerize u uniqueVarPrefix stmts =
  evalState (expandStatementsBUM (tryToRegisterize u) stmts) (uniqueVarPrefix, 0)

tryToRegisterize :: Int -> Statement -> State (String, Int) [Statement]
tryToRegisterize u stmt =
  case isLoop stmt of
    True -> return [stmt]
    False -> case isScalarOp u stmt || isRegisterizeableACCU u stmt of
      True -> registerizeStmt u stmt
      False -> return [stmt]

-- This is a hack. Registerization really check registerizeability on an operation
-- by operation basis
isRegisterizeableACCU u stmt =
  opcode stmt == ACCU && isScalar (operandWritten stmt) &&
  isScalar (operandRead 0 stmt) && isRegisterizeable u (operandRead 1 stmt)

registerizeBelow :: Int -> String -> [Statement] -> [Statement]
registerizeBelow u uniqueVarPrefix stmts =
  evalState (expandStatementsBUM (tryToRegisterizeBelow u) stmts) (uniqueVarPrefix, 0)

tryToRegisterizeBelow :: Int -> Statement -> State (String, Int) [Statement]
tryToRegisterizeBelow u stmt =
  case isLoop stmt of
    True -> return [stmt]
    False -> case isScalarOpBelow u stmt of
      True -> registerizeStmt u stmt
      False -> return [stmt]

registerizeStmt :: Int -> Statement -> State (String, Int) [Statement]
registerizeStmt i stmt =
  let u = iConst i in
  case opcode stmt of
    EADD -> registerizeMAdd u stmt
    SMUL -> registerizeSMul u stmt
    TRAN -> registerizeTrans u stmt
    MMUL -> registerizeMMul u stmt
    MSET -> registerizeTrans u stmt
    EMUL -> registerizeEMUL u stmt
    BRDC -> return [stmt]
    ZERO -> registerizeZERO u stmt
    ACCU -> registerizeACCU u stmt
    _ -> error $ "registerizeStmt: Unsupported operation " ++ show stmt

freshRegName :: State (String, Int) String
freshRegName = do
  (prefix, i) <- get
  put $ (prefix, i + 1)
  return $ prefix ++ show i

registerizeSymmetric op u stmt =
  do
    r1 <- toRegister u $ operandRead 0 stmt
    r2 <- toRegister u $ operandRead 1 stmt
    r3 <- toRegister u $ operandWritten stmt
    return [matrixSet r1 (operandRead 0 stmt), matrixSet r2 (operandRead 1 stmt), op r3 r1 r2, matrixSet (operandWritten stmt) r3]

toRegister u m =
  case isRegister m of
    True -> return m
    False -> do
      rName <- freshRegName
      return $ duplicateInRegister u rName m
  
registerizeMAdd u stmt =
  registerizeSymmetric matrixAdd u stmt

registerizeEMUL u stmt =
  registerizeSymmetric elemWiseMultiply u stmt

registerizeSMul u stmt =
  registerizeSymmetric scalarMultiply u stmt

registerizeMMul u stmt =
  let c = operandWritten stmt
      a = operandRead 0 stmt
      b = operandRead 1 stmt in
  do
    r1Name <- freshRegName
    r2Name <- freshRegName
    r3Name <- freshRegName
    let r1 = duplicateInRegister u r1Name a
        r2 = duplicateInRegister u r2Name b
        r3 = duplicateInRegister u r3Name c in
      return [matrixSet r1 a,
              matrixSet r2 b,
              matrixSet r3 c,
              matrixMultiply r3 r1 r2,
              matrixSet c r3]

registerizeACCU u stmt =
  let c = operandWritten stmt
      a = operandRead 0 stmt
      b = operandRead 1 stmt in
  do
    r1Name <- freshRegName
    r2Name <- freshRegName
    r3Name <- freshRegName
    let r1 = duplicateInRegister u r1Name a
        r2 = duplicateInRegister u r2Name b
        r3 = duplicateInRegister u r3Name c in
      return [matrixSet r1 a,
              matrixSet r2 b,
              matrixSet r3 c,
              accumulate r3 r1 r2,
              matrixSet c r3]

registerizeTrans u stmt =
  let a = operandWritten stmt
      b = operandRead 0 stmt in
  do
    r1Name <- freshRegName
    let r1 = duplicateInRegister u r1Name b in
      return [matrixSet r1 b, matrixSet a r1]

registerizeZERO u stmt =
  let a = operandWritten stmt in
  do
    r1Name <- freshRegName
    let r = duplicateInRegister u r1Name a in
      return [setZero r, matrixSet a r]
