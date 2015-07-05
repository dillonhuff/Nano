module CBackEnd.CodeGeneration.AVX(avxVarDecls, toAVX) where

import Data.List as L

import Analysis.Matrix
import CBackEnd.CodeGeneration.Common
import CBackEnd.Syntax
import CBackEnd.Utils
import IndexExpression
import Matrix
import Statement

avxVarDecls stmts = decls
  where
    iVarDecls = inductionVariableDecls stmts
    bufInfo = bufferInfoList stmts
    tempBufInfo = L.filter (\info -> bufScope info == local) bufInfo
    tempBufferDecls = bufDecls $ L.filter (\info -> isCPtr $ bufType info) tempBufInfo
    regs = L.filter (\info -> not $ isCPtr $ bufType info) tempBufInfo
    regDecls = L.map (\info -> (cM256Reg, bufName info)) regs
    decls = iVarDecls ++ regDecls ++ tempBufferDecls
  
toAVX stmt =
  case opcode stmt of
    LOOP -> loopToCStmts toAVX stmt
    _ -> toAVXIntrinsic stmt

toAVXIntrinsic stmt =
  firstToMatch avxInstructions stmt

fits_mm256_add_pd stmt =
  opcode stmt == EADD && allInRegister stmt && allVectorLEQ 4 stmt && allType double stmt

fits_mm256_mul_pd stmt =
  opcode stmt == EMUL && allInRegister stmt && allVectorLEQ 4 stmt && allType double stmt

fits_mm256_broadcast_sd stmt =
  opcode stmt == BRDC && isRegister (operandWritten stmt) &&
  not (isRegister (operandRead 0 stmt)) && isScalar (operandRead 0 stmt)

fits_assign stmt =
  opcode stmt == MSET && allInRegister stmt

fits_mm256_loadu_pd stmt =
  opcode stmt == MSET && allType double stmt &&
  isRegister (operandWritten stmt) &&
  isRegisterizeable 4 (operandWritten stmt) &&
  not (isRegister $ operandRead 0 stmt)

fits_mm256_storeu_pd stmt =
  opcode stmt == MSET && allType double stmt &&
  isRegister (operandRead 0 stmt) &&
  isRegisterizeable 4 (operandRead 0 stmt) &&
  not (isRegister $ operandWritten stmt)

fits_mm256_maskload_pd stmt =
  opcode stmt == MSET && allType double stmt &&
  isRegister (operandWritten stmt) &&
  isRegisterizeableBelow 4 (operandWritten stmt) &&
  not (isRegister $ operandRead 0 stmt)

fits_mm256_maskstore_pd stmt =
  opcode stmt == MSET && allType double stmt &&
  isRegister (operandRead 0 stmt) &&
  isRegisterizeableBelow 4 (operandRead 0 stmt) &&
  not (isRegister $ operandWritten stmt)


allInRegister stmt = L.all isRegister $ allOperands stmt

allVectorLEQ n stmt =
  (L.all (\m -> isVector m || isScalar m) $ allOperands stmt) && (L.all (\m -> max (constVal $ numRows m) (constVal $ numCols m) <= n) $ allOperands stmt)

allVectorEQ n stmt =
  (L.all isVector $ allOperands stmt) && (L.all (\m -> max (constVal $ numRows m) (constVal $ numCols m) == n) $ allOperands stmt)

allVectorLT n stmt =
  (L.all isVector $ allOperands stmt) && (L.all (\m -> max (constVal $ numRows m) (constVal $ numCols m) < n) $ allOperands stmt)

allType t stmt = L.all (\m -> dataType m == t) $ allOperands stmt

regWName stmt = regName $ operandWritten stmt
regName op = cVar $ bufferName op
regFuncall n stmt = cFuncall n $ L.map regName $ operandsRead stmt

matWExpr stmt = matToCExpr $ operandWritten stmt
matRExpr n stmt =
  case isRegister $ operandRead n stmt of
    True -> cVar $ bufferName $ operandRead n stmt
    False -> matToCExpr $ operandRead n stmt

fc n args = [cExprSt (cFuncall n args) ""]
afc lname fname args = [cExprSt (cAssign (cVar lname) (cFuncall fname args)) ""]

mask n =
  cFuncall "_mm256_set_epi32" $ maskArgs n

-- Also datatype depenent
maskArgs n =
  (L.replicate (2*(4 - n)) (cIntLit 0)) ++ (L.replicate (2*n) (cIntLit (-1)))

firstToMatch :: [(Statement -> Bool, Statement -> [CStmt String])] -> Statement -> [CStmt String]
firstToMatch [] stmt = error $ "firstToMatch: no matches for " ++ show stmt
firstToMatch ((cond, f):rest) stmt =
  if cond stmt then f stmt else firstToMatch rest stmt

avxInstructions =
  [(fits_mm256_add_pd, \stmt -> [cExprSt (cAssign (regWName stmt) (regFuncall "_mm256_add_pd" stmt)) ""]),
   (fits_mm256_mul_pd, \stmt -> [cExprSt (cAssign (regWName stmt) (regFuncall "_mm256_mul_pd" stmt)) ""]),
   (fits_mm256_broadcast_sd, \stmt -> [cExprSt (cAssign (regWName stmt) (cFuncall "_mm256_broadcast_sd" [matRExpr 0 stmt])) ""]),
   (fits_mm256_loadu_pd, \stmt -> [cExprSt (cAssign (regWName stmt) (cFuncall "_mm256_loadu_pd" [matRExpr 0 stmt])) ""]),
   (fits_mm256_storeu_pd, \stmt -> fc "_mm256_storeu_pd" [matWExpr stmt, matRExpr 0 stmt]),
   (fits_mm256_maskload_pd, \stmt -> [cExprSt (cAssign (regWName stmt) (cFuncall "_mm256_maskload_pd" [matRExpr 0 stmt, mask $ max (constVal $ numRows $ operandRead 0 stmt) (constVal $ numCols $ operandRead 0 stmt)])) ""]),
   (fits_mm256_maskstore_pd, \stmt -> fc "_mm256_maskstore_pd" [matWExpr stmt, mask $ max (constVal $ numRows $ operandWritten stmt) (constVal $ numCols $ operandWritten stmt), matRExpr 0 stmt]),
   (fits_assign, \stmt -> [cExprSt (cAssign (regWName stmt) (regName $ operandRead 0 stmt)) ""])]
