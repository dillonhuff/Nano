module MatrixOperation(MatrixOperation,
                       matrixOperation,
                       MatrixStmt,
                       masg, dmasg,
                       MExpr,
                       mBinop, mUnop, mat, dmBinop, dmUnop, dmat,
                       BOp(..), UOp(..)) where

import Text.Parsec.Pos

import FrontEnd.Token
import Matrix

data MatrixOperation
  = MatrixOperation String [MatrixStmt] SourcePos
    deriving (Ord, Show)

instance Eq MatrixOperation where
  (==) (MatrixOperation n1 stmts1 _) (MatrixOperation n2 stmts2 _) =
    n1 == n2 && stmts1 == stmts2

matrixOperation = MatrixOperation

data MatrixStmt
  = MAsg MExpr MExpr SourcePos
    deriving (Ord, Show)

masg = MAsg

dmasg m e = masg m e dummyPos

instance Eq MatrixStmt where
  (==) (MAsg lhs1 rhs1 _) (MAsg lhs2 rhs2 _) =
    lhs1 == lhs2 && rhs1 == rhs2

data MExpr
  = MBinop BOp MExpr MExpr SourcePos
  | MUnop UOp MExpr SourcePos
  | Mat Matrix SourcePos
    deriving (Ord, Show)

instance Eq MExpr where
  (==) (MBinop op1 l1 r1 _) (MBinop op2 l2 r2 _) =
    op1 == op2 && l1 == l2 && r1 == r2
  (==) (MUnop op1 l1 _) (MUnop op2 l2 _) =
    op1 == op2 && l1 == l2
  (==) (Mat m1 _) (Mat m2 _) = m1 == m2
  (==) _ _ = False

mBinop = MBinop
mUnop = MUnop
mat = Mat

dmBinop op l r = mBinop op l r dummyPos
dmUnop op m = mUnop op m dummyPos
dmat m = mat m dummyPos

data BOp
  = MAdd
  | SMul
  | MMul
  | MSub
    deriving (Eq, Ord, Show)

data UOp
  = MTrans
    deriving (Eq, Ord, Show)
