module Matrix(Matrix,
              matrix,
              subMatrix,
              numRows, numCols,
              properties,
              Type,
              single, double,
              arg, local) where

import IndexExpression

data Matrix
  = Matrix String IExpr IExpr IExpr IExpr Properties
  | SubMatrix IExpr IExpr IExpr IExpr Matrix Properties
    deriving (Eq, Ord, Show)

matrix = Matrix

subMatrix rStart numRows cStart numCols m@(Matrix name nr nc rs cs props) =
  SubMatrix rStart numRows cStart numCols m props

numRows (Matrix _ nr _ _ _ _) = nr
numRows (SubMatrix _ nr _ _ _ _) = nr

numCols (Matrix _ _ nc _ _ _) = nc
numCols (SubMatrix _ _ _ nc _ _) = nc

data Properties
  = Properties Scope Type
    deriving (Eq, Ord, Show)

properties = Properties

data Type
  = Single
  | Double
    deriving (Eq, Ord, Show)

single = Single
double = Double

data Scope
  = Arg
  | Local
    deriving (Eq, Ord, Show)

arg = Arg
local = Local
