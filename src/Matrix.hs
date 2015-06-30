module Matrix(Matrix,
              matrix,
              rowPart, colPart,
              isMatrix, isVector, isScalar,
              bufferName, locationExpr, sizeExpr,
              numRows, numCols, rowStride, colStride,
              properties, dataType, matrixBufferNameAndType,
              substituteInIExprs, partitionList, accessedRectangle,
              Type,
              single, double,
              isDouble, isSingle,
              arg, local, bufferScope) where

import Data.Map as M hiding (partition)

import Analysis.IndexExpression
import IndexExpression
import Scope

data Matrix
  = Matrix String IExpr IExpr IExpr IExpr Properties
  | SubMatrix Shape IExpr IExpr Matrix Properties
    deriving (Eq, Ord, Show)

matrix = Matrix

rowPart rStart numRows m =
  SubMatrix Row rStart numRows m (matProperties m)

colPart cStart numCols m =
  SubMatrix Col cStart numCols m (matProperties m)

isScalar m =
  numRows m == iConst 1 && numCols m == iConst 1

isVector m =
  not (isScalar m) && (numRows m == iConst 1 || numCols m == iConst 1)

isMatrix m =
  not (isScalar m) && not (isVector m)

matProperties (Matrix _ _ _ _ _ p) = p
matProperties (SubMatrix _ _ _ _ p) = p

bufferName (Matrix n _ _ _ _ _) = n
bufferName (SubMatrix _ _ _ m _) = bufferName m

bufferScope (Matrix _ _ _ _ _ p) = propScope p
bufferScope (SubMatrix _ _ _ m _) = bufferScope m

locationExpr (Matrix _ _ _ _ _ _) = iConst 0
locationExpr (SubMatrix Row rStart _ m _) =
  iAdd (iMul rStart (rowStride m)) $ locationExpr m
locationExpr (SubMatrix Col cStart _ m _) =
  iAdd (iMul cStart (colStride m)) $ locationExpr m

sizeExpr m@(Matrix _ _ _ _ _ _) =
  case rowStride m == iConst 1 || colStride m == iConst 1 of
    True -> evaluateIExprConstants $ iMul (numRows m) (numCols m)
    False -> evaluateIExprConstants $ iAdd (iMul (numRows m) (rowStride m)) (iMul (numCols m) (colStride m))
sizeExpr (SubMatrix _ _ _ m _) = sizeExpr m

accessedRectangle :: Map IExpr (IExpr, IExpr, IExpr) -> Matrix -> Maybe IRectangle
accessedRectangle iRanges (Matrix _ nr nc _ _ _) =
  case isConst nr && isConst nc of
    True -> Just $ iRectangle
                 (iRange (iConst 0) $ evaluateIExprConstants $ iSub nr (iConst 1))
                 (iRange (iConst 0) $ evaluateIExprConstants $ iSub nc (iConst 1))
    False -> Nothing
accessedRectangle iRanges (SubMatrix s r l m _) = do
  mR <- accessedRectangle iRanges m
  ar <- accessedRange (irStart $ rowRange mR) (irEnd $ rowRange mR) iRanges r l
  ac <- accessedRange (irStart $ colRange mR) (irEnd $ colRange mR) iRanges r l
  case s of
    Row -> Just $ iRectangle ar (colRange mR)
    Col -> Just $ iRectangle (rowRange mR) ac

accessedRange l m iRanges v b =
  case isConst v && isConst b of
    True -> Just $ iRange (evaluateIExprConstants (iAdd l v)) (evaluateIExprConstants (iSub (iAdd (iAdd l v) b) (iConst 1)))
    False -> case isConst b && isVar v of
      True -> varAccessedRange iRanges l v b
      False -> Nothing

varAccessedRange iRanges l v b =
  case M.lookup v iRanges of
    Just (s, i, e) -> case b == i of
      True -> Just $ iRange (evaluateIExprConstants $ iAdd l s) (evaluateIExprConstants $ iSub (iAdd (iAdd (iAdd l s) (iMul (iDiv (iSub e s) b) b)) b)  (iConst 1))
      False -> Nothing
    Nothing -> error $ "accessedRange: Cannot find " ++ show v ++ " in " ++ show iRanges

matrixBufferNameAndType (Matrix n _ _ _ _ (Properties _ t)) = (n, t)
matrixBufferNameAndType (SubMatrix _ _ _ m _) = matrixBufferNameAndType m

numRows (Matrix _ nr _ _ _ _) = nr
numRows (SubMatrix Row _ nr _ _) = nr
numRows (SubMatrix Col _ _ m _) = numRows m

numCols (Matrix _ _ nc _ _ _) = nc
numCols (SubMatrix Row _ _ m _) = numCols m
numCols (SubMatrix Col _ nc _ _) = nc

rowStride (Matrix _ _ _ rs _ _) = rs
rowStride (SubMatrix _ _ _ m _) = rowStride m

colStride (Matrix _ _ _ _ cs _) = cs
colStride (SubMatrix _ _ _ m _) = colStride m

dataType (Matrix _ _ _ _ _ p) = propType p
dataType (SubMatrix _ _ _ _ p) = propType p

substituteInIExprs target result (Matrix n nr nc rs cs p) =
  Matrix n (subIExpr target result nr) (subIExpr target result nc) (subIExpr target result rs) (subIExpr target result cs) p
substituteInIExprs target result (SubMatrix s i l m p) =
  SubMatrix s (subIExpr target result i) (subIExpr target result l) (substituteInIExprs target result m) p

partitionList (SubMatrix s i l m p) = (partition s i l) : (partitionList m)
partitionList _ = []

data Shape
  = Row
  | Col
    deriving (Eq, Ord, Show)

data Partition
  = Partition Shape IExpr IExpr
    deriving (Eq, Ord, Show)

partition s i l = Partition s i l

data Properties
  = Properties Scope Type
    deriving (Eq, Ord, Show)

properties = Properties

propScope (Properties s _) = s
propType (Properties _ t) = t

data Type
  = Single
  | Double
    deriving (Eq, Ord, Show)

single = Single
double = Double

isDouble Double = True
isDouble _ = False

isSingle t = not $ isDouble t
