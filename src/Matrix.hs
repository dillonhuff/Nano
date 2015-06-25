module Matrix(Matrix,
              matrix,
              rowPart, colPart,
              isMatrix, isVector, isScalar,
              bufferName, locationExpr, sizeExpr,
              numRows, numCols, rowStride, colStride,
              properties, dataType, matrixBufferNameAndType,
              Type,
              single, double,
              isDouble, isSingle,
              arg, local, bufferScope) where

import IndexExpression
import Scope

data Matrix
  = Matrix String IExpr IExpr IExpr IExpr Properties
  | SubMatrix IExpr IExpr IExpr IExpr Matrix Properties
    deriving (Eq, Ord, Show)

matrix = Matrix

subMatrix rStart numRows cStart numCols m@(Matrix name nr nc rs cs props) =
  SubMatrix rStart numRows cStart numCols m props
subMatrix rStart numRows cStart numCols m@(SubMatrix _ _ _ _ _ props) =
  SubMatrix rStart numRows cStart numCols m props

rowPart rStart numRows m =
  SubMatrix rStart numRows (iConst 0) (numCols m) m (matProperties m)

colPart cStart numCols m =
  SubMatrix (iConst 0) (numRows m) cStart numCols m (matProperties m)

isScalar m =
  numRows m == iConst 1 && numCols m == iConst 1

isVector m =
  not (isScalar m) && (numRows m == iConst 1 || numCols m == iConst 1)

isMatrix m =
  not (isScalar m) && not (isVector m)

matProperties (Matrix _ _ _ _ _ p) = p
matProperties (SubMatrix _ _ _ _ _ p) = p

bufferName (Matrix n _ _ _ _ _) = n
bufferName (SubMatrix _ _ _ _ m _) = bufferName m

bufferScope (Matrix _ _ _ _ _ p) = propScope p
bufferScope (SubMatrix _ _ _ _ m _) = bufferScope m

locationExpr (Matrix _ _ _ _ _ _) = iConst 0
locationExpr s@(SubMatrix rStart _ cStart _ m _) =
  iAdd (iAdd (iMul rStart (rowStride s)) (iMul cStart (colStride s))) $ locationExpr m

sizeExpr m@(Matrix _ _ _ _ _ _) =
  case rowStride m == iConst 1 || colStride m == iConst 1 of
    True -> evaluateIExprConstants $ iMul (numRows m) (numCols m)
    False -> evaluateIExprConstants $ iAdd (iMul (numRows m) (rowStride m)) (iMul (numCols m) (colStride m))
sizeExpr (SubMatrix _ _ _ _ m _) = sizeExpr m

matrixBufferNameAndType (Matrix n _ _ _ _ (Properties _ t)) = (n, t)
matrixBufferNameAndType (SubMatrix _ _ _ _ m _) = matrixBufferNameAndType m

numRows (Matrix _ nr _ _ _ _) = nr
numRows (SubMatrix _ nr _ _ _ _) = nr

numCols (Matrix _ _ nc _ _ _) = nc
numCols (SubMatrix _ _ _ nc _ _) = nc

rowStride (Matrix _ _ _ rs _ _) = rs
rowStride (SubMatrix _ _ _ _ m _) = rowStride m

colStride (Matrix _ _ _ _ cs _) = cs
colStride (SubMatrix _ _ _ _ m _) = colStride m

dataType (Matrix _ _ _ _ _ (Properties _ t)) = t
dataType (SubMatrix _ _ _ _ _ (Properties _ t)) = t

data Properties
  = Properties Scope Type
    deriving (Eq, Ord, Show)

properties = Properties

propScope (Properties s _) = s

data Type
  = Single
  | Double
    deriving (Eq, Ord, Show)

single = Single
double = Double

isDouble Double = True
isDouble _ = False

isSingle t = not $ isDouble t
