module Statement(Statement,
                 matrixMultiply, matrixAdd, loop, scalarMultiply,
                 isMatrixAdd, isMatrixMultiply, isLoop, isScalarMultiply,
                 loopStart, loopEnd, loopInc, loopInductionVariable, loopBody,
                 operandWritten, leftOperand, rightOperand,
                 applyToOperands,
                 collectFromAllOperands,
                 collectFromStmt, collectValuesFromStmt) where

import Data.List as L

import IndexExpression
import Matrix

data Statement
  = MatrixMultiply Matrix Matrix Matrix
  | ScalarMultiply Matrix Matrix Matrix
  | MatrixAdd Matrix Matrix Matrix
  | Loop String IExpr IExpr IExpr [Statement]
    deriving (Eq, Ord, Show)

matrixAdd = MatrixAdd
loop = Loop
matrixMultiply = MatrixMultiply
scalarMultiply = ScalarMultiply


applyToOperands :: (Matrix -> Matrix) -> Statement -> Statement
applyToOperands f (MatrixAdd c a b) = MatrixAdd (f c) (f a) (f b)
applyToOperands f (ScalarMultiply a alpha b) = ScalarMultiply (f a) (f alpha) (f b)

collectFromAllOperands :: (Matrix -> a) -> Statement -> [a]
collectFromAllOperands f (MatrixMultiply c a b) = [f c, f a, f b]
collectFromAllOperands f (MatrixAdd c a b) = [f c, f a, f b]
collectFromAllOperands f (ScalarMultiply c a b) = [f c, f a, f b]
collectFromAllOperands f (Loop _ _ _ _ _) = []

collectFromStmt :: (Statement -> a) -> Statement -> [a]
collectFromStmt f s@(Loop _ _ _ _ body) = (f s) : (L.concatMap (collectFromStmt f) body)
collectFromStmt f s = [f s]

collectValuesFromStmt :: (Statement -> [a]) -> Statement -> [a]
collectValuesFromStmt f s = L.concat $ collectFromStmt f s

isScalarMultiply (ScalarMultiply _ _ _) = True
isScalarMultiply _ = False

isMatrixAdd (MatrixAdd _ _ _) = True
isMatrixAdd _ = False

isMatrixMultiply (MatrixMultiply _ _ _) = True
isMatrixMultiply _ = False

isLoop (Loop _ _ _ _ _) = True
isLoop _ = False

loopStart (Loop _ s _ _ _) = s
loopEnd (Loop _ _ _ e _) = e
loopInc (Loop _ _ i _ _) = i
loopInductionVariable (Loop v _ _ _ _) = v
loopBody (Loop _ _ _ _ b) = b

operandWritten (MatrixAdd c _ _) = c
operandWritten (MatrixMultiply c _ _) = c
operandWritten (ScalarMultiply a _ _) = a

leftOperand (MatrixAdd _ a _) = a
leftOperand (MatrixMultiply _ a _) = a
leftOperand (ScalarMultiply _ a _) = a

rightOperand (MatrixAdd _ _ b) = b
rightOperand (MatrixMultiply _ _ b) = b
rightOperand (ScalarMultiply _ _ b) = b
