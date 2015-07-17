module Utils(duplicateInRegister, duplicateInTemp,
             mkRegister, applyOptimizations,
             packInRegister) where

import Core.IndexExpression
import Core.Matrix
import Core.Statement

duplicateInRegister u rName a =
  setName rName $ mkRegister u a

packInRegister u rName m =
  case (isRowVector m || isScalar m) && (constVal $ numCols m) <= (constVal u) of
    True -> setName rName $ setRegister $ matrix (bufferName m) (iConst 1) u (iConst 1) (iConst 1) (matProperties m)
    False -> case isColVector m && (constVal $ numRows m) <= (constVal u) of
      True -> setName rName $ setRegister $ matrix (bufferName m) u (iConst 1) (iConst 1) (iConst 1) (matProperties m)
      False -> error $ "packInRegister: Trying to pack illegal operand " ++ show m ++ " in to register of size " ++ show u

duplicateInTemp rName m =
  setLocal $ matrix rName (numRows m) (numCols m) (rowStride m) (colStride m) (matProperties m)

mkRegister u m = setRegister $ matrix (bufferName m) (numRows m) (numCols m) (rowStride m) (colStride m) (matProperties m)

applyOptimizations [] stmts = stmts
applyOptimizations (r:rest) stmts = r $ applyOptimizations rest stmts

