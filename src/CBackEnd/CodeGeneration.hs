module CBackEnd.CodeGeneration(operationToC,
                               bufferInfoList,
                               inductionVariableDecls) where

import Data.List as L

import CBackEnd.Syntax
import CBackEnd.Utils
import IndexExpression
import Matrix
import Statement

operationToC :: String -> [Statement] -> (CTopLevelItem String, [BufferInfo])
operationToC funcName stmts =
  (cFunction, argInfo) 
  where    
    bufInfo = bufferInfoList stmts
    tempBufInfo = L.filter (\info -> bufScope info == local) bufInfo
    tempBufferDecls = bufDecls tempBufInfo
    tempBufAllocation = L.map initializeBuffer $ L.filter (\info -> isCPtr $ bufType info) tempBufInfo
    body = tempBufAllocation ++ (L.concatMap toCStmts stmts)
    iVarDecls = inductionVariableDecls stmts
    localVarDecls = iVarDecls ++ tempBufferDecls
    argInfo = L.filter (\info -> bufScope info == arg) bufInfo
    argDecls = L.map (\info -> (bufType info, bufName info)) argInfo
    cFunction = cFuncDecl cVoid funcName argDecls (cBlock localVarDecls body)

toCStmts stmt =
  case not (isLoop stmt) && isScalarOp stmt of
    True -> toScalarC stmt
    _ -> toCStmtsMOp stmt

isScalarOp :: Statement -> Bool
isScalarOp stmt = L.all isScalar $ allOperands stmt

toScalarC stmt =
  case isMatrixAdd stmt of
    True -> scalarMAddToC stmt
    False -> case isScalarMultiply stmt of
      True -> scalarSMulToC stmt
      False -> case isMatrixMultiply stmt of
        True -> scalarMMulToC stmt
        False -> case isMatrixTranspose stmt || isMatrixSet stmt of
          True -> scalarMSetToC stmt
          False -> error $ "toCStmts: Unsupported statement " ++ show stmt

scalarMSetToC stmt =
  let b = rightOperand stmt
      a = operandWritten stmt in
  [cExprSt (cAssign (matrixLocExpr a) (matrixLocExpr b)) ""]

scalarMAddToC stmt =
  let c = operandWritten stmt
      a = leftOperand stmt
      b = rightOperand stmt in
  [cExprSt (cAssign (matrixLocExpr c) (cAdd (matrixLocExpr a) (matrixLocExpr b))) ""]

scalarMMulToC stmt =
  let c = operandWritten stmt
      a = leftOperand stmt
      b = rightOperand stmt in
  [cExprSt (cAssign (matrixLocExpr c) (cAdd (cMul (matrixLocExpr a) (matrixLocExpr b)) (matrixLocExpr c))) ""]

scalarSMulToC stmt =
  let c = operandWritten stmt
      alpha = leftOperand stmt
      b = rightOperand stmt in
  [cExprSt (cAssign (matrixLocExpr c) (cMul (matrixLocExpr alpha) (matrixLocExpr b))) ""]

toCStmtsMOp stmt =
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

matToCExpr m =
  cAdd (cVar $ bufferName m) (iExprToCExpr $ evaluateIExprConstants $ locationExpr m)

toCType :: Type -> CType
toCType t =
  case isDouble t of
    True -> cDouble
    False -> cFloat

inductionVariableDecls :: [Statement] -> [(CType, String)]
inductionVariableDecls stmts =
  let iNames = L.nub $ L.concatMap (collectValuesFromStmt (\st -> if isLoop st then [loopInductionVariable st] else [])) stmts in
  L.zip (L.replicate (length iNames) cInt) iNames

bufferInfoList :: [Statement] -> [BufferInfo]
bufferInfoList stmts =
  let allMats = L.nub $ L.concatMap (collectValuesFromStmt $ collectFromAllOperands matrixBufferInfo) stmts in
  L.sortBy (\l r -> compare (bufName l) (bufName r)) allMats

matrixBufferInfo :: Matrix -> BufferInfo
matrixBufferInfo m =
  case isRegister m of
    True -> bufferInfo (bufferName m) (toCType $ dataType m) (iExprToCExpr $ sizeExpr m) (bufferScope m)
    False -> bufferInfo (bufferName m) (cPtr $ toCType $ dataType m) (iExprToCExpr $ sizeExpr m) (bufferScope m)

matrixLocExpr m =
  case isRegister m of
    True -> cVar $ bufferName m
    False -> cArrAcc (cVar $ bufferName m) (iExprToCExpr $ locationExpr m)
