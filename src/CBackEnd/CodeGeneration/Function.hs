module CBackEnd.CodeGeneration.Function(toCStmtsFunction) where

import Data.List as L

import CBackEnd.Syntax
import IndexExpression
import Matrix
import Statement

toCStmtsFunction stmt =
  case isLoop stmt of
    True -> loopToCStmts stmt
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
              False -> error $ "toCStmts: Unsupported statement " ++ show stmt

loopToCStmts l =
  [cFor s e i b ""]
  where
    v = iExprToCExpr $ iVar $ loopInductionVariable l
    s = cAssign v (iExprToCExpr $ loopStart l)
    e = cLEQ v (iExprToCExpr $ loopEnd l)
    i = cAssign v (cAdd v (iExprToCExpr $ loopInc l))
    b = cBlock [] $ L.concatMap toCStmtsFunction $ loopBody l

matrixAddToCStmts madd =
  let c = operandWritten madd in
  case isDouble $ dataType c of
    True -> [cExprSt (cFuncall "simple_add" args) ""]
    False -> [cExprSt (cFuncall "simple_add_float" args) ""]
  where
    a = leftOperand madd
    b = rightOperand madd
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
    a = leftOperand madd
    b = rightOperand madd
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
    a = leftOperand madd
    b = rightOperand madd
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

matrixTransposeToCStmts madd =
  let a = operandWritten madd in
  case isDouble $ dataType a of
    True -> [cExprSt (cFuncall "simple_trans" args) ""]
    False -> [cExprSt (cFuncall "simple_trans_float" args) ""]
    where
      a = operandWritten madd
      b = rightOperand madd
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
      b = rightOperand mset
      m = iExprToCExpr $ numRows a
      n = iExprToCExpr $ numCols a
      aRS = iExprToCExpr $ rowStride a
      aCS = iExprToCExpr $ colStride a
      bRS = iExprToCExpr $ rowStride b
      bCS = iExprToCExpr $ colStride b
      args = [m, n,
              matToCExpr a, aRS, aCS,
              matToCExpr b, bRS, bCS]
  
matToCExpr m =
  case isRegister m of
    True -> cAddr (cVar $ bufferName m)
    False -> cAdd (cVar $ bufferName m) (iExprToCExpr $ evaluateIExprConstants $ locationExpr m)
