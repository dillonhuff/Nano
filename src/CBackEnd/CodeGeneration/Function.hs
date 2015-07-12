module CBackEnd.CodeGeneration.Function(toCStmtsFunction) where

import Data.List as L

import CBackEnd.CodeGeneration.Common
import CBackEnd.Syntax
import Core.IndexExpression
import Core.Matrix
import Core.Statement

toCStmtsFunction stmt =
  case isLoop stmt of
    True -> loopToCStmts toCStmtsFunction stmt
    False -> case isMatrixAdd stmt of
      True -> matrixAddToCStmts stmt
      False -> case isScalarMultiply stmt of
        True -> scalarMultiplyToCStmts stmt
        False -> case isMatrixMultiply stmt of
          True -> matrixMultiplyToCStmts stmt
          False -> case isMatrixTranspose stmt of
            True -> matrixTransposeToCStmts stmt
            False -> case isMatrixSet stmt of
              True -> matrixSetToCStmts stmt
              False -> case opcode stmt == ZERO of
                True -> setZeroToCStmts stmt
                False -> error $ "toCStmts: Unsupported statement " ++ show stmt

setZeroToCStmts setZ =
  let c = operandWritten setZ in
  case isDouble $ dataType c of
    True -> [cExprSt (cFuncall "set_to_zero_double" args) ""]
    False -> [cExprSt (cFuncall "set_to_zero_float" args) ""]
  where
    c = operandWritten setZ
    m = iExprToCExpr $ numRows c
    n = iExprToCExpr $ numCols c
    cRS = iExprToCExpr $ rowStride c
    cCS = iExprToCExpr $ colStride c
    args = [matToCExpr c,
            m, n, cRS, cCS]
  
matrixAddToCStmts madd =
  let c = operandWritten madd in
  case isDouble $ dataType c of
    True -> [cExprSt (cFuncall "simple_add" args) ""]
    False -> [cExprSt (cFuncall "simple_add_float" args) ""]
  where
    a = operandRead 0 madd
    b = operandRead 1 madd
    c = operandWritten madd
    m = iExprToCExpr $ numRows c
    n = iExprToCExpr $ numCols c
    aRS = iExprToCExpr $ rowStride a
    aCS = iExprToCExpr $ colStride a
    bRS = iExprToCExpr $ rowStride b
    bCS = iExprToCExpr $ colStride b
    cRS = iExprToCExpr $ rowStride c
    cCS = iExprToCExpr $ colStride c
    args = [m, n,
            matToCExpr a, aRS, aCS,
            matToCExpr b, bRS, bCS,
            matToCExpr c, cRS, cCS]

matrixMultiplyToCStmts madd =
  let c = operandWritten madd in
  case isDouble $ dataType c of
    True -> [cExprSt (cFuncall "simple_mmul" args) ""]
    False -> [cExprSt (cFuncall "simple_mmul_float" args) ""]
  where
    a = operandRead 0 madd
    b = operandRead 1 madd
    c = operandWritten madd
    m = iExprToCExpr $ numRows c
    n = iExprToCExpr $ numCols c
    p = iExprToCExpr $ numCols a
    aRS = iExprToCExpr $ rowStride a
    aCS = iExprToCExpr $ colStride a
    bRS = iExprToCExpr $ rowStride b
    bCS = iExprToCExpr $ colStride b
    cRS = iExprToCExpr $ rowStride c
    cCS = iExprToCExpr $ colStride c
    args = [m, n, p,
            matToCExpr a, aRS, aCS,
            matToCExpr b, bRS, bCS,
            matToCExpr c, cRS, cCS]

scalarMultiplyToCStmts madd =
  let c = operandWritten madd in
  case isDouble $ dataType c of
    True -> [cExprSt (cFuncall "simple_smul" args) ""]
    False -> [cExprSt (cFuncall "simple_smul_float" args) ""]
  where
    a = operandRead 0 madd
    b = operandRead 1 madd
    c = operandWritten madd
    m = iExprToCExpr $ numRows c
    n = iExprToCExpr $ numCols c
    bRS = iExprToCExpr $ rowStride b
    bCS = iExprToCExpr $ colStride b
    cRS = iExprToCExpr $ rowStride c
    cCS = iExprToCExpr $ colStride c
    args = [m, n,
            matToCExpr a,
            matToCExpr b, bRS, bCS,
            matToCExpr c, cRS, cCS]

matrixTransposeToCStmts trans =
  let a = operandWritten trans in
  case isDouble $ dataType a of
    True -> [cExprSt (cFuncall "simple_trans" args) ""]
    False -> [cExprSt (cFuncall "simple_trans_float" args) ""]
    where
      a = operandWritten trans
      b = operandRead 0 trans
      m = iExprToCExpr $ numRows a
      n = iExprToCExpr $ numCols a
      aRS = iExprToCExpr $ rowStride a
      aCS = iExprToCExpr $ colStride a
      bRS = iExprToCExpr $ rowStride b
      bCS = iExprToCExpr $ colStride b
      args = [m, n,
              matToCExpr a, aRS, aCS,
              matToCExpr b, bRS, bCS]

matrixSetToCStmts mset =
  let a = operandWritten mset in
  case isDouble $ dataType a of
    True -> [cExprSt (cFuncall "copy_double" args) ""]
    False -> [cExprSt (cFuncall "copy_float" args) ""]
    where
      a = operandWritten mset
      b = operandRead 0 mset
      m = iExprToCExpr $ numRows a
      n = iExprToCExpr $ numCols a
      aRS = iExprToCExpr $ rowStride a
      aCS = iExprToCExpr $ colStride a
      bRS = iExprToCExpr $ rowStride b
      bCS = iExprToCExpr $ colStride b
      args = [m, n,
              matToCExpr a, aRS, aCS,
              matToCExpr b, bRS, bCS]
