module CBackEnd.CodeGeneration.AVX.CodeSnippets(mask,
                                                maskDbl,
                                                rrbroadcast,
                                                accum4) where

import Data.List as L

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
  [cExprSt (cFuncall "RRBROADCAST" [cVar $ bufferName $ a, cVar $ bufferName c]) ""]

--      t0 = cFuncall "_mm256_unpacklo_pd" [cVar $ bufferName $ a, cVar $ bufferName a]
--      t1 = cFuncall "_mm256_permute2f128_pd" [t0, t0, cVar "0b00100010"] in
{-  [cExprSt (cAssign (cVar $ bufferName c) t1) ""]-}

accum4 stmt =
  let c = operandWritten stmt
      a = operandRead 0 stmt
      b = operandRead 1 stmt
      t4 = cFuncall "_mm256_hadd_pd" [cVar $ bufferName b, cFuncall "_mm256_setzero_pd" []]
      t5 = cFuncall "_mm256_permute4x64_pd" [t4, cVar "0b11011000"]
      t6 = cFuncall "_mm256_hadd_pd" [t5, cFuncall "_mm256_setzero_pd" []]
      t7 = cFuncall "_mm256_add_pd" [t6, cVar $ bufferName a] in
  [cExprSt (cAssign (cVar $ bufferName c) t7) ""]


--  [cExprSt (cFuncall "ACCUM4" [cVar $ bufferName $ a, cVar $ bufferName $ b, cVar $ bufferName c]) ""]-}
