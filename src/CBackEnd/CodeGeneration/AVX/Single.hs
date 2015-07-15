module CBackEnd.CodeGeneration.AVX.Single(avxVarDeclsSingle, stmtsToAVXSingle) where

import Data.List as L

import Analysis.Matrix
import CBackEnd.CodeGeneration.Common
import CBackEnd.Syntax
import CBackEnd.Utils
import Core.IndexExpression
import Core.Matrix
import Core.Statement

avxVarDeclsSingle stmts = decls
  where
    iVarDecls = inductionVariableDecls stmts
    bufInfo = bufferInfoList stmts
    tempBufInfo = L.filter (\info -> bufScope info == local) bufInfo
    tempBufferDecls = bufDecls $ L.filter (\info -> isCPtr $ bufType info) tempBufInfo
    regs = L.filter (\info -> not $ isCPtr $ bufType info) tempBufInfo
    regDecls = L.map (\info -> (cM256dReg, bufName info)) regs
    decls = iVarDecls ++ regDecls ++ tempBufferDecls

stmtsToAVXSingle stmts =
  L.concatMap toAVXSingle stmts

toAVXSingle stmt =
  case opcode stmt of
    LOOP -> loopToCStmts toAVXSingle stmt
    _ -> toAVXIntrinsic stmt

toAVXIntrinsic stmt =
  firstToMatch avxInstructions stmt

fits_mm256_add_ps stmt =
  opcode stmt == EADD && allInRegister stmt && allVectorLEQ 8 stmt && allType single stmt

fits_mm256_mul_ps stmt =
  opcode stmt == EMUL && allInRegister stmt && allVectorLEQ 8 stmt && allType single stmt

fits_mm256_fmadd_ps stmt =
  opcode stmt == MMUL && allInRegister stmt &&
  allVectorLEQ 8 stmt && allType single stmt &&
  isScalar (operandWritten stmt)

fits_mm256_broadcast_ss stmt =
  opcode stmt == BRDC && isRegister (operandWritten stmt) &&
  not (isRegister (operandRead 0 stmt)) && isScalar (operandRead 0 stmt) &&
  allType single stmt

fits_assign stmt =
  opcode stmt == MSET && allInRegister stmt

fits_mm256_loadu_ps stmt =
  opcode stmt == PACK && allType single stmt &&
  isContiguous (operandRead 0 stmt) &&
  isRegister (operandWritten stmt) &&
  isRegisterizeable 8 (operandWritten stmt) &&
  not (isRegister $ operandRead 0 stmt)

fits_mm256_storeu_ps stmt =
  opcode stmt == UNPK && allType single stmt &&
  isRegister (operandRead 0 stmt) &&
  isContiguous (operandWritten stmt) &&
  isRegisterizeable 8 (operandRead 0 stmt) &&
  not (isRegister $ operandWritten stmt)

fits_mm256_maskload_ps stmt =
  opcode stmt == PACK && allType single stmt &&
  isRegister (operandWritten stmt) &&
  isContiguous (operandRead 0 stmt) &&
  isRegisterizeableBelow 8 (operandWritten stmt) &&
  not (isRegister $ operandRead 0 stmt)

fits_mm256_maskstore_ps stmt =
  opcode stmt == UNPK && allType single stmt &&
  isRegister (operandRead 0 stmt) &&
  isContiguous (operandWritten stmt) &&
  isRegisterizeableBelow 8 (operandRead 0 stmt) &&
  not (isRegister $ operandWritten stmt)

fits_mm256_setzero_ps stmt =
  opcode stmt == ZERO && allType single stmt &&
  isRegister (operandWritten stmt) &&
  (isRegisterizeableBelow 8 (operandWritten stmt) || isRegisterizeable 8 (operandWritten stmt))

fits_accum4 stmt =
  opcode stmt == ACCU && allType single stmt &&
  isRegister (operandWritten stmt) && isRegister (operandRead 1 stmt) &&
  isRegisterizeable 1 (operandWritten stmt) && isRegisterizeable 8 (operandRead 1 stmt)

fits_accumBelow4 stmt =
  opcode stmt == ACCU && allType single stmt &&
  isRegister (operandWritten stmt) && isRegister (operandRead 1 stmt) &&
  isRegisterizeable 1 (operandWritten stmt) && isRegisterizeableBelow 8 (operandRead 1 stmt)

fits_rrbroadcast stmt =
  opcode stmt == BRDC && allInRegister stmt &&
  isScalar (operandRead 0 stmt) && allType single stmt

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
  (L.replicate (8 - n) (cIntLit 0)) ++ (L.replicate n (cIntLit (-1)))

avxInstructions =
  [(fits_mm256_add_ps, \stmt -> [cExprSt (cAssign (regWName stmt) (regFuncall "_mm256_add_ps" stmt)) ""]),
   (fits_mm256_mul_ps, \stmt -> [cExprSt (cAssign (regWName stmt) (regFuncall "_mm256_mul_ps" stmt)) ""]),
   (fits_mm256_fmadd_ps, \stmt -> [cExprSt (cAssign (regWName stmt) (regFuncall "_mm256_fmadd_ps" stmt)) ""]),
   (fits_mm256_setzero_ps, \stmt -> afc (bufferName $ operandWritten stmt) "_mm256_setzero_ps" []),
   (fits_mm256_broadcast_ss, \stmt -> [cExprSt (cAssign (regWName stmt) (cFuncall "_mm256_broadcast_ss" [matRExpr 0 stmt])) ""]),
   (fits_mm256_loadu_ps, \stmt -> [cExprSt (cAssign (regWName stmt) (cFuncall "_mm256_loadu_ps" [matRExpr 0 stmt])) ""]),
   (fits_mm256_storeu_ps, \stmt -> fc "_mm256_storeu_ps" [matWExpr stmt, matRExpr 0 stmt]),
   (fits_mm256_maskload_ps, \stmt -> [cExprSt (cAssign (regWName stmt) (cFuncall "_mm256_maskload_ps" [matRExpr 0 stmt, mask $ max (constVal $ numRows $ operandRead 0 stmt) (constVal $ numCols $ operandRead 0 stmt)])) ""]),
   (fits_mm256_maskstore_ps, \stmt -> fc "_mm256_maskstore_ps" [matWExpr stmt, mask $ max (constVal $ numRows $ operandWritten stmt) (constVal $ numCols $ operandWritten stmt), matRExpr 0 stmt]),
--   (fits_accum4, \stmt -> accum4 stmt),
--   (fits_rrbroadcast, \stmt -> rrbroadcast stmt),
--   (fits_accumBelow4, \stmt -> accumBelow4 stmt),
   (fits_assign, \stmt -> [cExprSt (cAssign (regWName stmt) (regName $ operandRead 0 stmt)) ""])]
