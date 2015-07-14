module Transformations.IntroducePacking(pack) where

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
    False -> if allOperandsInMemory stmt then packStmt u stmt else error $ "tryToPack: " ++ show stmt

-- This is a hack. Registerization really check packability on an operation
-- by operation basis
isPackable u stmt =
  isScalarOpBelow u stmt || isScalarOp u stmt

isPackableACCU u stmt =
  (isPackable u stmt) ||
  (opcode stmt == ACCU && isScalar (operandWritten stmt) &&
   isScalar (operandRead 0 stmt) && isRegisterizeable u (operandRead 1 stmt))

isPackableBRDC u stmt =
  isPackable u stmt ||
  (opcode stmt == BRDC && isScalar (operandRead 0 stmt) &&
   isRegisterizeable u (operandWritten stmt))

isPackableEADD u stmt =
  isPackable u stmt

isPackableEMUL u stmt =
  isPackable u stmt

isPackableSMUL u stmt =
  isPackable u stmt

isPackableTRAN u stmt =
  isPackable u stmt

isPackableMMUL u stmt =
  isPackable u stmt

isPackableMSET u stmt =
  isPackable u stmt

isPackableZERO u stmt =
  isPackable u stmt

packStmt :: Int -> Statement -> State (String, Int) [Statement]
packStmt i stmt =
  let u = iConst i in
  case opcode stmt of
    EADD -> if isPackableEADD i stmt then packMAdd u stmt else return [stmt]
    SMUL -> if isPackableSMUL i stmt then packSMul u stmt else return [stmt]
    TRAN -> if isPackableTRAN i stmt then packTrans u stmt else return [stmt]
    MMUL -> if isPackableMMUL i stmt then packMMul u stmt else return [stmt]
    MSET -> if isPackableMSET i stmt then packTrans u stmt else return [stmt]
    EMUL -> if isPackableEMUL i stmt then packEMUL u stmt else return [stmt]
    BRDC -> if isPackableBRDC i stmt then packBRDC u stmt else return [stmt]
    ZERO -> if isPackableZERO i stmt then packZERO u stmt else return [stmt]
    ACCU -> if isPackableACCU i stmt then packACCU u stmt else return [stmt]
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
