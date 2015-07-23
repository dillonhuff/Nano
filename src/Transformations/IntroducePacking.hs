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

isPackableFMA u stmt =
  opcode stmt == FMA &&
  isPackable u stmt

isPackableMSET u stmt =
  isPackable u stmt

isPackableZERO u stmt =
  isPackable u stmt

packStmt :: Int -> Statement -> State (String, Int) [Statement]
packStmt i stmt =
  let u = iConst i in
  case opcode stmt of
    EADD -> if isPackableEADD i stmt then packEAdd u stmt else return [stmt]
    SMUL -> if isPackableSMUL i stmt then packSMul u stmt else return [stmt]
    TRAN -> if isPackableTRAN i stmt then packTrans u stmt else return [stmt]
    FMA -> if isPackableFMA i stmt then packFMA u stmt else return [stmt]
    MSET -> if isPackableMSET i stmt then packTrans u stmt else return [stmt]
    EMUL -> if isPackableEMUL i stmt then packEMUL u stmt else return [stmt]
    BRDC -> if isPackableBRDC i stmt then packBRDC u stmt else return [stmt]
    ZERO -> if isPackableZERO i stmt then packZERO u stmt else return [stmt]
    ACCU -> if isPackableACCU i stmt then packACCU u stmt else return [stmt]
    _ -> error $ "packStmt: Unsupported operation " ++ show stmt

packEAdd u stmt =
  packSymmetric matrixAdd u stmt

packEMUL u stmt =
  packSymmetric elemWiseMultiply u stmt

packSMul u stmt =
  packSymmetric scalarMultiply u stmt

packFMA u stmt = packTripleOp fusedMultiplyAdd u stmt

packACCU u stmt = packTripleOp accumulate u stmt

packTrans u stmt =
  let a = operandWritten stmt
      b = operandRead 0 stmt in
  do
    r1 <- packToRegister u b
    return [matrixPack r1 b, matrixUnpack a r1]

packZERO u stmt =
  do
    r <- packToRegister u $ operandWritten stmt
    return [setZero r, matrixUnpack (operandWritten stmt) r]

packBRDC u stmt =
  let a = operandWritten stmt
      b = operandRead 0 stmt in
  do
    r <- packToRegister u $ a
    return [broadcast r b, matrixUnpack a r]

packSymmetric op u stmt =
  do
    r1 <- packToRegister u $ operandRead 0 stmt
    r2 <- packToRegister u $ operandRead 1 stmt
    r3 <- packToRegister u $ operandWritten stmt
    return [matrixPack r1 (operandRead 0 stmt),
            matrixPack r2 (operandRead 1 stmt),
            op r3 r1 r2,
            matrixUnpack (operandWritten stmt) r3]

packTripleOp op u stmt =
  let c = operandWritten stmt
      a = operandRead 0 stmt
      b = operandRead 1 stmt in
  do
    r1 <- packToRegister u a
    r2 <- packToRegister u b
    r3 <- packToRegister u c
    return [matrixPack r1 a,
            matrixPack r2 b,
            matrixPack r3 c,
            op r3 r1 r2,
            matrixUnpack c r3]

packToRegister u m =
  case isRegister m of
    True -> return m
    False -> do
      rName <- freshRegName
      return $ packInRegister u rName m
  
freshRegName :: State (String, Int) String
freshRegName = do
  (prefix, i) <- get
  put $ (prefix, i + 1)
  return $ prefix ++ show i

