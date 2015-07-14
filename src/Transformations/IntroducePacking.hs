module Transformations.IntroducePacking(pack,
                                        packBelow) where

import Control.Monad.State
import Data.List as L

import Analysis.Matrix
import Analysis.Statement
import Core.IndexExpression
import Core.Matrix
import Core.Statement
import Utils

pack :: Int -> String -> [Statement] -> [Statement]
pack u uniqueVarPrefix stmts =
  evalState (expandStatementsBUM (tryToPack u) stmts) (uniqueVarPrefix, 0)

tryToPack :: Int -> Statement -> State (String, Int) [Statement]
tryToPack u stmt =
  case isLoop stmt of
    True -> return [stmt]
    False -> case isScalarOp u stmt || isPackableACCU u stmt ||
             isPackableBRDC u stmt of
      True -> packStmt u stmt
      False -> return [stmt]

-- This is a hack. Registerization really check packability on an operation
-- by operation basis
isPackableACCU u stmt =
  opcode stmt == ACCU && isScalar (operandWritten stmt) &&
  isScalar (operandRead 0 stmt) && isRegisterizeable u (operandRead 1 stmt)

isPackableBRDC u stmt =
  opcode stmt == BRDC && isScalar (operandRead 0 stmt) &&
  isRegisterizeable u (operandWritten stmt)

packBelow :: Int -> String -> [Statement] -> [Statement]
packBelow u uniqueVarPrefix stmts =
  evalState (expandStatementsBUM (tryToPackBelow u) stmts) (uniqueVarPrefix, 0)

tryToPackBelow :: Int -> Statement -> State (String, Int) [Statement]
tryToPackBelow u stmt =
  case isLoop stmt of
    True -> return [stmt]
    False -> case isScalarOpBelow u stmt of
      True -> packStmt u stmt
      False -> return [stmt]

packStmt :: Int -> Statement -> State (String, Int) [Statement]
packStmt i stmt =
  let u = iConst i in
  case opcode stmt of
    EADD -> packMAdd u stmt
    SMUL -> packSMul u stmt
    TRAN -> packTrans u stmt
    MMUL -> packMMul u stmt
    MSET -> packTrans u stmt
    EMUL -> packEMUL u stmt
    BRDC -> packBRDC u stmt 
    ZERO -> packZERO u stmt
    ACCU -> packACCU u stmt
    _ -> error $ "packStmt: Unsupported operation " ++ show stmt

freshRegName :: State (String, Int) String
freshRegName = do
  (prefix, i) <- get
  put $ (prefix, i + 1)
  return $ prefix ++ show i

packSymmetric op u stmt =
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
  
packMAdd u stmt =
  packSymmetric matrixAdd u stmt

packEMUL u stmt =
  packSymmetric elemWiseMultiply u stmt

packSMul u stmt =
  packSymmetric scalarMultiply u stmt

packMMul u stmt =
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

packACCU u stmt =
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

packTrans u stmt =
  let a = operandWritten stmt
      b = operandRead 0 stmt in
  do
    r1Name <- freshRegName
    let r1 = duplicateInRegister u r1Name b in
      return [matrixSet r1 b, matrixSet a r1]

packZERO u stmt =
  let a = operandWritten stmt in
  do
    r1Name <- freshRegName
    let r = duplicateInRegister u r1Name a in
      return [setZero r, matrixSet a r]

packBRDC u stmt =
  let a = operandWritten stmt
      b = operandRead 0 stmt in
  do
    r1Name <- freshRegName
    let r1 = duplicateInRegister u r1Name a in
      return [broadcast r1 b, matrixSet a r1]
