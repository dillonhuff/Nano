module MatrixOperation(MatrixStmt,
                       masg,
                       MExpr,
                       mBinop, mUnop, mat,
                       BOp(..), UOp(..)) where

import Text.Parsec.Pos

import Matrix

data MatrixStmt
  = MAsg Matrix MExpr SourcePos
    deriving (Eq, Ord, Show)

masg = MAsg

data MExpr
  = MBinop BOp MExpr MExpr SourcePos
  | MUnop UOp MExpr MExpr SourcePos
  | Mat Matrix SourcePos
    deriving (Eq, Ord, Show)

mBinop = MBinop
mUnop = MUnop
mat = Mat

data BOp
  = MAdd
  | SMul
  | MMul
  | MSub
    deriving (Eq, Ord, Show)

data UOp
  = MTrans
    deriving (Eq, Ord, Show)
