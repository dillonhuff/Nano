module MatrixOperation(MatrixOperation,
                       matrixOperation,
                       MatrixStmt,
                       masg, dmasg,
                       MExpr,
                       mBinop, mUnop, mat, dmBinop, dmUnop, dmat,
                       BOp(..), UOp(..),
                       linearizeStmts) where

import Control.Monad
import Control.Monad.State
import Data.List as L
import Text.Parsec.Pos

import FrontEnd.Token
import IndexExpression
import Matrix
import Statement

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

linearizeStmts :: String -> [MatrixStmt] -> [Statement]
linearizeStmts tempNamePrefix matStmts =
  evalState (linearizeStmtsM matStmts) (tempNamePrefix, 0)

linearizeStmtsM :: [MatrixStmt] -> State (String, Int) [Statement]
linearizeStmtsM matStmts = do
  resStmts <- liftM L.concat $ sequence $ L.map linearizeStmtM matStmts
  return resStmts

linearizeStmtM :: MatrixStmt -> State (String, Int) [Statement]
linearizeStmtM (MAsg w e _) = do
  (eRes, eStmts) <- linearizeMExpr e
  (wRes, _) <- linearizeMExpr w
  return $ eStmts ++ [matrixSet wRes eRes]

linearizeMExpr :: MExpr -> State (String, Int) (Matrix, [Statement])
linearizeMExpr (MBinop MAdd l r _) = do
  (lr, lStmts) <- linearizeMExpr l
  (rr, rStmts) <- linearizeMExpr r
  t <- freshTemp lr
  return $ (t, lStmts ++ rStmts ++ [matrixAdd t lr rr])
linearizeMExpr (MBinop SMul l r _) = do
  (lr, lStmts) <- linearizeMExpr l
  (rr, rStmts) <- linearizeMExpr r
  t <- freshTemp rr
  return $ (t, lStmts ++ rStmts ++ [scalarMultiply t lr rr])
linearizeMExpr (MBinop MMul l r _) = do
  (lr, lStmts) <- linearizeMExpr l
  (rr, rStmts) <- linearizeMExpr r
  t <- freshTemp $ matrix (bufferName lr) (numRows lr) (numCols rr) (iConst 1) (numRows lr) (matProperties lr)
  return $ (t, lStmts ++ rStmts ++ [setZero t, matrixMultiply t lr rr])
linearizeMExpr (MUnop MTrans m _) = do
  (mr, mStmts) <- linearizeMExpr m
  t <- freshTemp $ matrix (bufferName mr) (numCols mr) (numRows mr) (iConst 1) (numCols mr) (matProperties mr)
  return $ (t, mStmts ++ [matrixTranspose t mr])  
linearizeMExpr (Mat m _) = return (m, [])

freshTemp m = do
  n <- freshName
  return $ setName n $ setLocal m

freshName :: State (String, Int) String
freshName = do
  (prefix, i) <- get
  put $ (prefix, i + 1)
  return $ prefix ++ show i
