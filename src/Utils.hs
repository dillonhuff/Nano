module Utils(duplicateInRegister, mkRegister, applyOptimizations) where

import IndexExpression
import Matrix
import Statement

duplicateInRegister u rName a =
  setName rName $ mkRegister u a

mkRegister u m = setRegister $ matrix (bufferName m) (numRows m) (numCols m) (rowStride m) (colStride m) (matProperties m)

applyOptimizations :: [[Statement] -> [Statement]] -> [Statement] -> [Statement]
applyOptimizations [] stmts = stmts
applyOptimizations (r:rest) stmts = r $ applyOptimizations rest stmts


{-  case isRowVector m of
    True -> setRegister $ matrix (bufferName m) (iConst 1) u u (iConst 1) (matProperties m)
    False -> setRegister $ matrix (bufferName m) u (iConst 1) (iConst 1) u (matProperties m)-}
