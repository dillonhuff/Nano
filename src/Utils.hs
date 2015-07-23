module Utils(duplicateInTemp,
             applyOptimizations,
             packInRegister,
             packInRegisterGroup) where

import Analysis.Matrix
import Core.IndexExpression
import Core.Matrix
import Core.MemLocation
import Core.Statement

packInRegister u rName m =
  case (isRowVector m || isScalar m) && (constVal $ numCols m) <= (constVal u) of
    True -> setName rName $ setRegister ByRow $ matrix (bufferName m) (iConst 1) u (iConst 1) (iConst 1) (matProperties m)
    False -> case isColVector m && (constVal $ numRows m) <= (constVal u) of
      True -> setName rName $ setRegister ByCol $ matrix (bufferName m) u (iConst 1) (iConst 1) (iConst 1) (matProperties m)
      False -> error $ "packInRegister: Trying to pack illegal operand " ++ show m ++ " in to register of size " ++ show u

packInRegisterGroup m n rName op =
  case isLEQ (constVal m) (constVal n) op of
    True ->
      setName rName $ setRegister (regStorageFormat op) $ matrix (bufferName op) m n n (iConst 1) (matProperties op)
    False -> error $ "packInRegisterGroup: Trying to pack " ++
             show op ++ " into a " ++ show m ++ " by " ++
             show n ++ "register group"
  
duplicateInTemp rName m =
  setLocal $ matrix rName (numRows m) (numCols m) (rowStride m) (colStride m) (matProperties m)

regStorageFormat op =
  if isRowMajor op then ByRow else if isColMajor op then ByCol else error $ "regStorageFormat: Trying to pack " ++ show op

applyOptimizations [] stmts = stmts
applyOptimizations (r:rest) stmts = r $ applyOptimizations rest stmts
