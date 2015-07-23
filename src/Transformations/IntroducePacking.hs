module Transformations.IntroducePacking(pack) where

import Control.Monad.State
import Data.List as L

import Analysis.Matrix
import Analysis.Statement
import Core.IndexExpression
import Core.Matrix
import Core.Statement
import Matching
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
  opcode stmt == ACCU &&
  (isPackable u stmt ||
   (isScalar (operandWritten stmt) &&
    isScalar (operandRead 0 stmt) && isRegisterizeable u (operandRead 1 stmt)))

isPackableBRDC u stmt =
  opcode stmt == BRDC &&
  (isPackable u stmt ||
  (isScalar (operandRead 0 stmt) &&
   isRegisterizeable u (operandWritten stmt)))

isPackableEADD u stmt =
  opcode stmt == EADD &&
  isPackable u stmt

isPackableSquareEADD u stmt =
  opcode stmt == EADD &&
  isRegisterGroupOp u u stmt

isPackableEMUL u stmt =
  opcode stmt == EMUL &&
  isPackable u stmt

isPackableSMUL u stmt =
  opcode stmt == SMUL &&
  isPackable u stmt

isPackableTRAN u stmt =
  opcode stmt == TRAN &&
  isPackable u stmt

isPackableFMA u stmt =
  opcode stmt == FMA &&
  isPackable u stmt

isPackableMSET u stmt =
  opcode stmt == MSET &&
  isPackable u stmt

isPackableZERO u stmt =
  opcode stmt == ZERO &&
  isPackable u stmt

packStmt :: Int -> Statement -> State (String, Int) [Statement]
packStmt i stmt =
  let u = iConst i in
  firstToMatch (possiblePackings i u) stmt

possiblePackings i u =
  [(isPackableEADD i, packEAdd u),
   (isPackableSquareEADD i, packSquareEADD u),
   (isPackableSMUL i, packSMul u),
   (isPackableTRAN i, packTrans u), 
   (isPackableFMA i, packFMA u),
   (isPackableMSET i, packTrans u),
   (isPackableEMUL i, packEMUL u),
   (isPackableBRDC i, packBRDC u),
   (isPackableZERO i, packZERO u),
   (isPackableACCU i, packACCU u)]

packEAdd u stmt = packSymmetric matrixAdd u stmt

packSquareEADD u stmt = do
  r1 <- packToRegisterGroup u u $ operandRead 0 stmt
  r2 <- packToRegisterGroup u u $ operandRead 1 stmt
  r3 <- packToRegisterGroup u u $ operandWritten stmt
  return [matrixPack r1 (operandRead 0 stmt),
          matrixPack r2 (operandRead 1 stmt),
          matrixAdd r3 r1 r2,
          matrixUnpack (operandWritten stmt) r3]  

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

packToRegisterGroup m n op =
  case isRegister op of
    True -> return op
    False -> do
      rName <- freshRegName
      return $ packInRegisterGroup m n rName op

freshRegName :: State (String, Int) String
freshRegName = do
  (prefix, i) <- get
  put $ (prefix, i + 1)
  return $ prefix ++ show i
