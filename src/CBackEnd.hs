module CBackEnd(operationToC) where

import Data.List as L

import CGen
import IndexExpression
import Matrix
import Statement

operationToC :: String -> [Statement] -> CTopLevelItem String
operationToC funcName stmts =
  cFuncDecl cVoid funcName [] (cBlock [] body)
  where
    body = L.concatMap toCStmts stmts

toCStmts stmt =
  case isLoop stmt of
    True -> loopToCStmts stmt
    False -> case isMatrixAdd stmt of
      True -> matrixAddToCStmts stmt
      False -> error $ "toCStmts: Unsupported statement " ++ show stmt

loopToCStmts l =
  [cFor s e i b ""]
  where
    v = iExprToCExpr $ iVar $ loopInductionVariable l
    s = cAssign v (iExprToCExpr $ loopStart l)
    e = cLEQ v (iExprToCExpr $ loopEnd l)
    i = cAssign v (cAdd v (iExprToCExpr $ loopInc l))
    b = cBlock [] $ L.concatMap toCStmts $ loopBody l

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
            matToCExpr c, cRS, cCS,
            matToCExpr a, aRS, aCS,
            matToCExpr b, bRS, bCS]

matToCExpr m =
  cAdd (cVar $ bufferName m) (iExprToCExpr $ evaluateIExprConstants $ locationExpr m)
