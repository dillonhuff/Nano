module Utils(duplicateInRegister, mkRegister, applyOptimizations) where

import Core.IndexExpression
import Core.Matrix
import Core.Statement

duplicateInRegister u rName a =
  setName rName $ mkRegister u a

mkRegister u m = setRegister $ matrix (bufferName m) (numRows m) (numCols m) (rowStride m) (colStride m) (matProperties m)

applyOptimizations [] stmts = stmts
applyOptimizations (r:rest) stmts = r $ applyOptimizations rest stmts

