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

accum4 stmt =
  let c = operandWritten stmt
      a = operandRead 0 stmt
      b = operandRead 1 stmt in
  [cExprSt (cFuncall "ACCUM4" [cVar $ bufferName $ a, cVar $ bufferName $ b, cVar $ bufferName c]) ""]
