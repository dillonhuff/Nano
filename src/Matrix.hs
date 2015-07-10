module Matrix(Matrix,
              matrix,
              rowPart, colPart,
              isMatrix, isVector, isScalar, isRowVector, isColVector, isNull,
              isRowMajor, isColMajor, isContiguous, isFixedSize,
              bufferName, locationExpr, sizeExpr, matProperties,
              numRows, numCols, rowStride, colStride,
              properties, dataType, matrixBufferNameAndType,
              substituteInIExprs, partitionList, accessedRectangle,
              setName, setRegister, isRegister, underlyingMatrix, baseMatrix,
              replaceSupermatrix, smallestSubsumingPartition, partitionedBy,
              Type,
              single, double,
              isDouble, isSingle,
              arg, local, bufferScope,
              register, memory) where

import Data.Map as M hiding (partition)

import Analysis.IndexExpression
import IndexExpression
import Partition
import Scope
import Type

data Matrix
  = Matrix String IExpr IExpr IExpr IExpr Properties
  | SubMatrix Shape IExpr IExpr Matrix Properties
    deriving (Eq, Ord)

instance Show Matrix where
  show (Matrix n nr nc rs cs p) = n ++ "[" ++
                                  show nr ++ " " ++
                                  show nc ++ " " ++
                                  show rs ++ " " ++
                                  show cs ++ "]" ++
                                  show p
  show (SubMatrix s v b m p) = show s ++ " " ++
                               show v ++ " " ++ show b ++ " (" ++ show m ++ ") " ++ show p

matrix = Matrix

rowPart rStart numRows m =
  SubMatrix Row rStart numRows m (matProperties m)

colPart cStart numCols m =
  SubMatrix Col cStart numCols m (matProperties m)

isNull m =
  numRows m == iConst 0 || numCols m == iConst 0

isScalar m =
  numRows m == iConst 1 && numCols m == iConst 1

isVector m =
  not (isScalar m) && (numRows m == iConst 1 || numCols m == iConst 1)

isRowVector m =
  isVector m && numRows m == iConst 1

isColVector m =
  isVector m && numCols m == iConst 1

isMatrix m =
  not (isScalar m) && not (isVector m)

isColMajor m =
  rowStride m == iConst 1

isRowMajor m =
  colStride m == iConst 1

isContiguous m =
  isScalar m || (isRowVector m && isRowMajor m) || (isColVector m && isColMajor m)

isFixedSize m =
  isConst (numRows m) && isConst (numCols m)

underlyingMatrix (SubMatrix _ _ _ m _) = m
underlyingMatrix m = m

baseMatrix (SubMatrix _ _ _ m _) = baseMatrix m
baseMatrix m = m

replaceSupermatrix target result s@(SubMatrix shp v b m p) =
  case s == target of
    True -> result
    False -> SubMatrix shp v b (replaceSupermatrix target result m) p
replaceSupermatrix target result m =
  case m == target of
    True -> result
    False -> m

smallestSubsumingPartition m n@(Matrix _ _ _ _ _ _) =
  case underlyingMatrix m == n of
    True -> Just n
    False -> Nothing
smallestSubsumingPartition m n =
  case m == n of
    True -> Just $ m
    False -> smallestSubsumingPartition m (peelPartition n)

peelPartition (SubMatrix _ _ _ m _) = m
    
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

matrixBufferNameAndType (Matrix n _ _ _ _ (Properties _ t _)) = (n, t)
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

setName n (Matrix _ nr nc rs rc p) = Matrix n nr nc rs rc p
setName n (SubMatrix shp v l m p) = SubMatrix shp v l (setName n m) p

setRegister (Matrix n nr nc rs rc (Properties s t l)) =
  Matrix n nr nc rs rc (Properties local t register)
setRegister (SubMatrix shp v l m (Properties s t _)) =
  SubMatrix shp v l (setRegister m) (Properties local t register)

isRegister (Matrix _ _ _ _ _ p) = propMemLocation p == register
isRegister (SubMatrix _ _ _ _ p) = propMemLocation p == register

partitionedBy i (Matrix _ nr nc rs cs _) =
  containsSubExpr i nr || containsSubExpr i nc || containsSubExpr i rs || containsSubExpr i cs
partitionedBy i (SubMatrix _ v b m p) =
  (containsSubExpr i v || containsSubExpr i b) || partitionedBy i m

data Properties
  = Properties Scope Type MemLocation
    deriving (Eq, Ord)

instance Show Properties where
  show (Properties s t m) = "{" ++ show s ++ " " ++ show t ++ " " ++ show m ++ "}"

properties = Properties

propScope (Properties s _ _) = s
propType (Properties _ t _) = t
propMemLocation (Properties _ _ l) = l

data MemLocation
  = Memory
  | Register
    deriving (Eq, Ord)

instance Show MemLocation where
  show Memory = "M"
  show Register = "R"

memory = Memory
register = Register
