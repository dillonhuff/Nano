module CBackEnd.CodeGeneration.AVX.Common(avxSingleInstructions) where

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
  opcode stmt == MMUL && allInRegister stmt &&
  allVectorEQ len stmt && allType tp stmt &&
  isScalar (operandWritten stmt)

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

mask n =
  cFuncall "_mm256_set_epi32" $ maskArgs n

-- Also datatype depenent
maskArgs n =
  (L.replicate (8 - n) (cIntLit 0)) ++ (L.replicate n (cIntLit (-1)))
