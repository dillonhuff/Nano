module CBackEnd.CodeGeneration.AVX.Double(avxVarDeclsDouble, toAVXDouble) where

import Data.List as L

import Analysis.Matrix
import CBackEnd.CodeGeneration.Common
import CBackEnd.Syntax
import CBackEnd.Utils
import IndexExpression
import Matrix
import Statement

avxVarDeclsDouble stmts = decls
  where
    iVarDecls = inductionVariableDecls stmts
    bufInfo = bufferInfoList stmts
    tempBufInfo = L.filter (\info -> bufScope info == local) bufInfo
    tempBufferDecls = bufDecls $ L.filter (\info -> isCPtr $ bufType info) tempBufInfo
    regs = L.filter (\info -> not $ isCPtr $ bufType info) tempBufInfo
    regDecls = L.map (\info -> (cM256dReg, bufName info)) regs
    decls = iVarDecls ++ regDecls ++ tempBufferDecls
  
toAVXDouble stmt =
  case opcode stmt of
    LOOP -> loopToCStmts toAVXDouble stmt
    _ -> toAVXIntrinsic stmt

toAVXIntrinsic stmt =
  firstToMatch avxInstructions stmt

fits_mm256_add_pd stmt =
  opcode stmt == EADD && allInRegister stmt && allVectorLEQ 4 stmt && allType double stmt

fits_mm256_mul_pd stmt =
  opcode stmt == EMUL && allInRegister stmt && allVectorLEQ 4 stmt && allType double stmt

fits_mm256_fmadd_pd stmt =
  opcode stmt == MMUL && allInRegister stmt &&
  allVectorLEQ 4 stmt && allType double stmt &&
  isScalar (operandWritten stmt)

fits_mm256_broadcast_sd stmt =
  opcode stmt == BRDC && isRegister (operandWritten stmt) &&
  not (isRegister (operandRead 0 stmt)) && isScalar (operandRead 0 stmt) &&
  allType double stmt

fits_assign stmt =
  opcode stmt == MSET && allInRegister stmt

fits_mm256_loadu_pd stmt =
  opcode stmt == MSET && allType double stmt &&
  isContiguous (operandRead 0 stmt) &&
  isRegister (operandWritten stmt) &&
  isRegisterizeable 4 (operandWritten stmt) &&
  not (isRegister $ operandRead 0 stmt)

fits_mm256_storeu_pd stmt =
  opcode stmt == MSET && allType double stmt &&
  isRegister (operandRead 0 stmt) &&
  isContiguous (operandWritten stmt) &&
  isRegisterizeable 4 (operandRead 0 stmt) &&
  not (isRegister $ operandWritten stmt)

fits_mm256_maskload_pd stmt =
  opcode stmt == MSET && allType double stmt &&
  isRegister (operandWritten stmt) &&
  isContiguous (operandRead 0 stmt) &&
  isRegisterizeableBelow 4 (operandWritten stmt) &&
  not (isRegister $ operandRead 0 stmt)

fits_mm256_maskstore_pd stmt =
  opcode stmt == MSET && allType double stmt &&
  isRegister (operandRead 0 stmt) &&
  isContiguous (operandWritten stmt) &&
  isRegisterizeableBelow 4 (operandRead 0 stmt) &&
  not (isRegister $ operandWritten stmt)

fits_mm256_setzero_pd stmt =
  opcode stmt == ZERO && allType double stmt &&
  isRegister (operandWritten stmt) &&
  (isRegisterizeableBelow 4 (operandWritten stmt) || isRegisterizeable 4 (operandWritten stmt))

fits_accum4 stmt =
  opcode stmt == ACCU && allType double stmt &&
  isRegister (operandWritten stmt) && isRegister (operandRead 1 stmt) &&
  isRegisterizeable 1 (operandWritten stmt) && isRegisterizeable 4 (operandRead 1 stmt)

fits_accumBelow4 stmt =
  opcode stmt == ACCU && allType double stmt &&
  isRegister (operandWritten stmt) && isRegister (operandRead 1 stmt) &&
  isRegisterizeable 1 (operandWritten stmt) && isRegisterizeableBelow 4 (operandRead 1 stmt)

fits_rrbroadcast stmt =
  opcode stmt == BRDC && allInRegister stmt &&
  isScalar (operandRead 0 stmt) && allType double stmt

rrbroadcast stmt =
  let c = operandWritten stmt
      a = operandRead 0 stmt
      t0 = cFuncall "_mm256_unpacklo_pd" [cVar $ bufferName $ a, cVar $ bufferName a]
      t1 = cFuncall "_mm256_permute2f128_pd" [t0, t0, cVar "0b00100010"] in
  [cExprSt (cAssign (cVar $ bufferName c) t1) ""]

accumBelow4 stmt =
  let c = operandWritten stmt
      a = operandRead 0 stmt
      b = operandRead 1 stmt
      t1 = cFuncall "_mm256_blendv_pd" [cFuncall "_mm256_setzero_pd" [], cVar $ bufferName b, mask $ max (constVal $ numRows $ operandRead 1 stmt) (constVal $ numCols $ operandRead 1 stmt)]
      t4 = cFuncall "_mm256_hadd_pd" [t1, cFuncall "_mm256_setzero_pd" []]
      t5 = cFuncall "_mm256_permute4x64_pd" [t4, cVar "0b11011000"]
      t6 = cFuncall "_mm256_hadd_pd" [t5, cFuncall "_mm256_setzero_pd" []]
      t7 = cFuncall "_mm256_add_pd" [t6, cVar $ bufferName a] in
  [cExprSt (cAssign (cVar $ bufferName c) t7) ""]  
  
accum4 stmt =
  let c = operandWritten stmt
      a = operandRead 0 stmt
      b = operandRead 1 stmt
      t4 = cFuncall "_mm256_hadd_pd" [cVar $ bufferName b, cFuncall "_mm256_setzero_pd" []]
      t5 = cFuncall "_mm256_permute4x64_pd" [t4, cVar "0b11011000"]
      t6 = cFuncall "_mm256_hadd_pd" [t5, cFuncall "_mm256_setzero_pd" []]
      t7 = cFuncall "_mm256_add_pd" [t6, cVar $ bufferName a] in
  [cExprSt (cAssign (cVar $ bufferName c) t7) ""]

mask n =
  cFuncall "_mm256_set_epi32" $ maskArgs n

-- Also datatype depenent
maskArgs n =
  (L.replicate (2*(4 - n)) (cIntLit 0)) ++ (L.replicate (2*n) (cIntLit (-1)))

avxInstructions =
  [(fits_mm256_add_pd, \stmt -> [cExprSt (cAssign (regWName stmt) (regFuncall "_mm256_add_pd" stmt)) ""]),
   (fits_mm256_mul_pd, \stmt -> [cExprSt (cAssign (regWName stmt) (regFuncall "_mm256_mul_pd" stmt)) ""]),
   (fits_mm256_fmadd_pd, \stmt -> [cExprSt (cAssign (regWName stmt) (regFuncall "_mm256_fmadd_pd" stmt)) ""]),
   (fits_mm256_setzero_pd, \stmt -> afc (bufferName $ operandWritten stmt) "_mm256_setzero_pd" []),
   (fits_mm256_broadcast_sd, \stmt -> [cExprSt (cAssign (regWName stmt) (cFuncall "_mm256_broadcast_sd" [matRExpr 0 stmt])) ""]),
   (fits_mm256_loadu_pd, \stmt -> [cExprSt (cAssign (regWName stmt) (cFuncall "_mm256_loadu_pd" [matRExpr 0 stmt])) ""]),
   (fits_mm256_storeu_pd, \stmt -> fc "_mm256_storeu_pd" [matWExpr stmt, matRExpr 0 stmt]),
   (fits_mm256_maskload_pd, \stmt -> [cExprSt (cAssign (regWName stmt) (cFuncall "_mm256_maskload_pd" [matRExpr 0 stmt, mask $ max (constVal $ numRows $ operandRead 0 stmt) (constVal $ numCols $ operandRead 0 stmt)])) ""]),
   (fits_mm256_maskstore_pd, \stmt -> fc "_mm256_maskstore_pd" [matWExpr stmt, mask $ max (constVal $ numRows $ operandWritten stmt) (constVal $ numCols $ operandWritten stmt), matRExpr 0 stmt]),
   (fits_accum4, \stmt -> accum4 stmt),
   (fits_rrbroadcast, \stmt -> rrbroadcast stmt),
   (fits_accumBelow4, \stmt -> accumBelow4 stmt),
   (fits_assign, \stmt -> [cExprSt (cAssign (regWName stmt) (regName $ operandRead 0 stmt)) ""])]
