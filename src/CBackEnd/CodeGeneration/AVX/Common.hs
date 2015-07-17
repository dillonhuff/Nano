module CBackEnd.CodeGeneration.AVX.Common(avxSingleInstructions,
                                          avxDoubleInstructions) where

import Data.List as L

import Analysis.Matrix
import CBackEnd.CodeGeneration.Common
import CBackEnd.Syntax
import CBackEnd.Utils
import Core.IndexExpression
import Core.Matrix
import Core.Statement

fits_mm256_add len tp stmt =
  opcode stmt == EADD && allInRegister stmt && allVectorEQ len stmt && allType tp stmt

fits_mm256_mul len tp stmt =
  opcode stmt == EMUL && allInRegister stmt && allVectorEQ len stmt && allType tp stmt

fits_mm256_fmadd len tp stmt =
  opcode stmt == FMA && allInRegister stmt &&
  allVectorEQ len stmt && allType tp stmt

fits_mm256_broadcast len tp stmt =
  opcode stmt == BRDC && isRegister (operandWritten stmt) &&
  not (isRegister (operandRead 0 stmt)) && isScalar (operandRead 0 stmt) &&
  allType tp stmt

fits_assign stmt =
  opcode stmt == MSET && allInRegister stmt

fits_mm256_loadu len tp stmt =
  opcode stmt == PACK && allType tp stmt &&
  isContiguous (operandRead 0 stmt) &&
  isRegister (operandWritten stmt) &&
  isRegisterizeable len (operandWritten stmt) &&
  isRegisterizeable len (operandRead 0 stmt) &&
  not (isRegister $ operandRead 0 stmt)

fits_mm256_storeu len tp stmt =
  opcode stmt == UNPK && allType tp stmt &&
  isRegister (operandRead 0 stmt) &&
  isContiguous (operandWritten stmt) &&
  isRegisterizeable len (operandWritten stmt) &&
  isRegisterizeable len (operandRead 0 stmt) &&
  not (isRegister $ operandWritten stmt)

fits_mm256_maskload len tp stmt =
  opcode stmt == PACK && allType tp stmt &&
  isRegister (operandWritten stmt) &&
  isContiguous (operandRead 0 stmt) &&
  isRegisterizeableBelow len (operandRead 0 stmt) &&
  not (isRegister $ operandRead 0 stmt)

fits_mm256_maskstore len tp stmt =
  opcode stmt == UNPK && allType tp stmt &&
  isRegister (operandRead 0 stmt) &&
  isContiguous (operandWritten stmt) &&
  isRegisterizeableBelow len (operandWritten stmt) &&
  not (isRegister $ operandWritten stmt)

fits_mm256_setzero len tp stmt =
  opcode stmt == ZERO && allType tp stmt &&
  isRegister (operandWritten stmt) &&
  (isRegisterizeableBelow len (operandWritten stmt) || isRegisterizeable len (operandWritten stmt))

mask n =
  cFuncall "_mm256_set_epi32" $ maskArgs n

-- Also datatype depenent
maskArgs n =
  (L.replicate (8 - n) (cIntLit 0)) ++ (L.replicate n (cIntLit (-1)))

maskDbl n =
  cFuncall "_mm256_set_epi32" $ maskDblArgs n

maskDblArgs n =
  (L.replicate (2*(4 - n)) (cIntLit 0)) ++ (L.replicate (2*n) (cIntLit (-1)))

fits_rrbroadcast stmt =
  opcode stmt == BRDC && allInRegister stmt &&
  isScalar (operandRead 0 stmt) && allType double stmt

rrbroadcast stmt =
  let c = operandWritten stmt
      a = operandRead 0 stmt
      t0 = cFuncall "_mm256_unpacklo_pd" [cVar $ bufferName $ a, cVar $ bufferName a]
      t1 = cFuncall "_mm256_permute2f128_pd" [t0, t0, cVar "0b00100010"] in
  [cExprSt (cAssign (cVar $ bufferName c) t1) ""]

fits_accum4 stmt =
  opcode stmt == ACCU && allType double stmt &&
  isRegister (operandWritten stmt) && isRegister (operandRead 1 stmt) &&
  allInRegister stmt && allVectorEQ 4 stmt

accum4 stmt =
  let c = operandWritten stmt
      a = operandRead 0 stmt
      b = operandRead 1 stmt
      t4 = cFuncall "_mm256_hadd_pd" [cVar $ bufferName b, cFuncall "_mm256_setzero_pd" []]
      t5 = cFuncall "_mm256_permute4x64_pd" [t4, cVar "0b11011000"]
      t6 = cFuncall "_mm256_hadd_pd" [t5, cFuncall "_mm256_setzero_pd" []]
      t7 = cFuncall "_mm256_add_pd" [t6, cVar $ bufferName a] in
  [cExprSt (cAssign (cVar $ bufferName c) t7) ""]

avxSingleInstructions =
  [(fits_mm256_add 8 single, \stmt -> [cExprSt (cAssign (regWName stmt) (regFuncall "_mm256_add_ps" stmt)) ""]),
   (fits_mm256_mul 8 single, \stmt -> [cExprSt (cAssign (regWName stmt) (regFuncall "_mm256_mul_ps" stmt)) ""]),
   (fits_mm256_fmadd 8 single, \stmt -> [cExprSt (cAssign (regWName stmt) (regFuncall "_mm256_fmadd_ps" stmt)) ""]),
   (fits_mm256_setzero 8 single, \stmt -> afc (bufferName $ operandWritten stmt) "_mm256_setzero_ps" []),
   (fits_mm256_broadcast 8 single, \stmt -> [cExprSt (cAssign (regWName stmt) (cFuncall "_mm256_broadcast_ss" [matRExpr 0 stmt])) ""]),
   (fits_mm256_loadu 8 single, \stmt -> [cExprSt (cAssign (regWName stmt) (cFuncall "_mm256_loadu_ps" [matRExpr 0 stmt])) ""]),
   (fits_mm256_storeu 8 single, \stmt -> fc "_mm256_storeu_ps" [matWExpr stmt, matRExpr 0 stmt]),
   (fits_mm256_maskload 8 single, \stmt -> [cExprSt (cAssign (regWName stmt) (cFuncall "_mm256_maskload_ps" [matRExpr 0 stmt, mask $ max (constVal $ numRows $ operandRead 0 stmt) (constVal $ numCols $ operandRead 0 stmt)])) ""]),
   (fits_mm256_maskstore 8 single, \stmt -> fc "_mm256_maskstore_ps" [matWExpr stmt, mask $ max (constVal $ numRows $ operandWritten stmt) (constVal $ numCols $ operandWritten stmt), matRExpr 0 stmt]),
   (fits_assign, \stmt -> [cExprSt (cAssign (regWName stmt) (regName $ operandRead 0 stmt)) ""])]

avxDoubleInstructions =
  [(fits_mm256_add 4 double, \stmt -> [cExprSt (cAssign (regWName stmt) (regFuncall "_mm256_add_pd" stmt)) ""]),
   (fits_mm256_mul 4 double, \stmt -> [cExprSt (cAssign (regWName stmt) (regFuncall "_mm256_mul_pd" stmt)) ""]),
   (fits_mm256_fmadd 4 double, \stmt -> [cExprSt (cAssign (regWName stmt) (regFuncall "_mm256_fmadd_pd" stmt)) ""]),
   (fits_mm256_setzero 4 double, \stmt -> afc (bufferName $ operandWritten stmt) "_mm256_setzero_pd" []),
   (fits_mm256_broadcast 4 double, \stmt -> [cExprSt (cAssign (regWName stmt) (cFuncall "_mm256_broadcast_sd" [matRExpr 0 stmt])) ""]),
   (fits_mm256_loadu 4 double, \stmt -> [cExprSt (cAssign (regWName stmt) (cFuncall "_mm256_loadu_pd" [matRExpr 0 stmt])) ""]),
   (fits_mm256_storeu 4 double, \stmt -> fc "_mm256_storeu_pd" [matWExpr stmt, matRExpr 0 stmt]),
   (fits_mm256_maskload 4 double, \stmt -> [cExprSt (cAssign (regWName stmt) (cFuncall "_mm256_maskload_pd" [matRExpr 0 stmt, maskDbl $ max (constVal $ numRows $ operandRead 0 stmt) (constVal $ numCols $ operandRead 0 stmt)])) ""]),
   (fits_mm256_maskstore 4 double, \stmt -> fc "_mm256_maskstore_pd" [matWExpr stmt, maskDbl $ max (constVal $ numRows $ operandWritten stmt) (constVal $ numCols $ operandWritten stmt), matRExpr 0 stmt]),
   (fits_accum4, \stmt -> accum4 stmt),
   (fits_rrbroadcast, \stmt -> rrbroadcast stmt),
   (fits_assign, \stmt -> [cExprSt (cAssign (regWName stmt) (regName $ operandRead 0 stmt)) ""])]

