module CBackEnd.CodeGeneration.AVX.OpcodeTests(fits_mm256_add,
                                               fits_mm256_mul,
                                               fits_mm256_fmadd,
                                               fits_mm256_broadcast,
                                               fits_mm256_loadu,
                                               fits_mm256_storeu,
                                               fits_mm256_maskload,
                                               fits_mm256_maskstore,
                                               fits_mm256_setzero,
                                               fits_assign,
                                               fits_accum4,
                                               fits_rrbroadcast) where

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

fits_rrbroadcast stmt =
  opcode stmt == BRDC && allInRegister stmt &&
  allType double stmt

fits_accum4 stmt =
  opcode stmt == ACCU && allType double stmt &&
  isRegister (operandWritten stmt) && isRegister (operandRead 1 stmt) &&
  allInRegister stmt && allVectorEQ 4 stmt

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
