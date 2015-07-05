module Registerization(registerize,
                       registerizeBelow) where

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
  case isLoop stmt of
    True -> return [stmt]
    False -> case isScalarOp u stmt of
      True -> registerizeStmt u stmt
      False -> return [stmt]

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
registerizeStmt u stmt =
  case opcode stmt of
    EADD -> registerizeMAdd stmt
    SMUL -> registerizeSMul stmt
    TRAN -> registerizeTrans stmt
    MMUL -> registerizeMMul stmt
    MSET -> registerizeTrans stmt
    EMUL -> registerizeEMUL u stmt
    BRDC -> return [stmt]
    _ -> error $ "registerizeStmt: Unsupported operation " ++ show stmt

freshRegName :: State (String, Int) String
freshRegName = do
  (prefix, i) <- get
  put $ (prefix, i + 1)
  return $ prefix ++ show i

registerizeSymmetric op u stmt =
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
      return [matrixSet r1 a, matrixSet r2 b, op r3 r1 r2, matrixSet c r3]
  
registerizeMAdd stmt =
  registerizeSymmetric matrixAdd 1 stmt

registerizeEMUL u stmt =
  registerizeSymmetric elemWiseMultiply 1 stmt

registerizeSMul stmt =
  registerizeSymmetric scalarMultiply 1 stmt

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
      return [matrixSet r1 a,
              matrixSet r2 b,
              matrixSet r3 c,
              matrixMultiply r3 r1 r2,
              matrixSet c r3]

registerizeTrans stmt =
  let a = operandWritten stmt
      b = operandRead 0 stmt in
  do
    r1Name <- freshRegName
    let r1 = duplicateInRegister r1Name b in
      return [matrixSet r1 b, matrixSet a r1]
  
duplicateInRegister rName a =
  setName rName $ mkRegister a

mkRegister m =
  setRegister $ matrix (bufferName m) (numRows m) (numCols m) (rowStride m) (colStride m) (matProperties m)
