module CBackEnd.CodeGeneration.AVX.CodeSnippets(mask,
                                                maskDbl,
                                                rrbroadcast,
                                                accum4,
                                                unpack_dbl_4x4,
                                                pack_dbl_4x4,
                                                eadd_dbl_4x4) where

import Data.List as L

import CBackEnd.CodeGeneration.AVX.CodeGenState
import CBackEnd.CodeGeneration.Common
import CBackEnd.Syntax
import CBackEnd.Utils
import Core.IndexExpression
import Core.Matrix
import Core.Statement

mask n =
  cFuncall "_mm256_set_epi32" $ maskArgs n

-- Also datatype depenent
maskArgs n =
  (L.replicate (8 - n) (cIntLit 0)) ++ (L.replicate n (cIntLit (-1)))

maskDbl n =
  cFuncall "_mm256_set_epi32" $ maskDblArgs n

maskDblArgs n =
  (L.replicate (2*(4 - n)) (cIntLit 0)) ++ (L.replicate (2*n) (cIntLit (-1)))

rrbroadcast stmt =
  let c = operandWritten stmt
      a = operandRead 0 stmt in
  do
    t0 <- freshTempVar
    return [cExprSt (cFuncall "RRBROADCAST" [t0, cVar $ bufferName $ a, cVar $ bufferName c]) ""]

accum4 stmt =
  let c = operandWritten stmt
      a = operandRead 0 stmt
      b = operandRead 1 stmt in
  do
    t0 <- freshTempVar
    t1 <- freshTempVar
    t2 <- freshTempVar
    return [cExprSt (cFuncall "ACCUM4" [t0, t1, t2, cVar $ bufferName $ a, cVar $ bufferName $ b, cVar $ bufferName c]) ""]

unpack_dbl_4x4 stmt =
  return [cExprSt (cVar "UNPACK_DBL_4x4") ""]

pack_dbl_4x4 stmt =
  return [cExprSt (cVar "PACK_DBL_4x4") ""]

eadd_dbl_4x4 stmt =
  return [cExprSt (cVar "EADD_DBL_4x4") ""]
