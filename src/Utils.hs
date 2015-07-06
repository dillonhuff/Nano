module Utils(duplicateInRegister, mkRegister) where

import IndexExpression
import Matrix

duplicateInRegister u rName a =
  setName rName $ mkRegister u a

mkRegister u m = setRegister $ matrix (bufferName m) (numRows m) (numCols m) (rowStride m) (colStride m) (matProperties m)

{-  case isRowVector m of
    True -> setRegister $ matrix (bufferName m) (iConst 1) u u (iConst 1) (matProperties m)
    False -> setRegister $ matrix (bufferName m) u (iConst 1) (iConst 1) u (matProperties m)-}
