module Registerization(registerize,
                       registerizeBelow) where

import Control.Monad.State
import Data.List as L

import Analysis.Matrix
import Analysis.Statement
import IndexExpression
import Matrix
import Statement
import Utils

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
    let r1 = duplicateInRegister u r1Name a
        r2 = duplicateInRegister u r2Name b
        r3 = duplicateInRegister u r3Name c in
      return [matrixSet r1 a, matrixSet r2 b, op r3 r1 r2, matrixSet c r3]
  
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

registerizeTrans u stmt =
  let a = operandWritten stmt
      b = operandRead 0 stmt in
  do
    r1Name <- freshRegName
    let r1 = duplicateInRegister u r1Name b in
      return [matrixSet r1 b, matrixSet a r1]
  
{-
mkRegister u m =
  case isRowVector m of
    True -> setRegister $ matrix (bufferName m) (iConst 1) u (rowStride m) (colStride m) (matProperties m)
    False -> setRegister $ matrix (bufferName m) u (iConst 1) (rowStride m) (colStride m) (matProperties m)
-}
{-
mkRegister u m =
  setRegister $ matrix (bufferName m) (if numRows m == u then u else iConst 1) (if numCols m == u then u else iConst 1) (rowStride m) (colStride m) (matProperties m)
-}
