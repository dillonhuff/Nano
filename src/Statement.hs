module Statement(Statement,
                 matrixMultiply, matrixTranspose, matrixAdd, loop, scalarMultiply, matrixSet,
                 isMatrixAdd, isMatrixTranspose, isMatrixMultiply, isLoop, isScalarMultiply, isMatrixSet,
                 loopStart, loopEnd, loopInc, loopInductionVariable, loopBody,
                 operandWritten, leftOperand, rightOperand, allOperands, operandsRead,
                 expandStatementBU, expandStatementsBU, expandStatementBUM, expandStatementsBUM,
                 applyToOperands, applyToStatementBU, applyToLoopBodiesBU,
                 collectFromAllOperands,
                 collectFromStmt, collectValuesFromStmt) where

import Control.Monad
import Data.List as L

import IndexExpression
import Matrix

data Statement
  = MatrixMultiply Matrix Matrix Matrix
  | ScalarMultiply Matrix Matrix Matrix
  | MatrixTranspose Matrix Matrix
  | MatrixAdd Matrix Matrix Matrix
  | MatrixSet Matrix Matrix
  | Loop String IExpr IExpr IExpr [Statement]
    deriving (Eq, Ord, Show)

matrixSet = MatrixSet
matrixAdd = MatrixAdd
loop = Loop
matrixMultiply = MatrixMultiply
matrixTranspose = MatrixTranspose
scalarMultiply = ScalarMultiply

expandStatementsBUM :: (Monad m) => (Statement -> m [Statement]) -> [Statement] -> m [Statement]
expandStatementsBUM f stmts = liftM L.concat $ sequence $ L.map (expandStatementBUM f) stmts

expandStatementBUM :: (Monad m) => (Statement -> m [Statement]) -> Statement -> m [Statement]
expandStatementBUM f (Loop v s i e body) = do
  bodyM <- expandStatementsBUM f body
  f $ Loop v s i e bodyM
expandStatementBUM f s = f s

expandStatementsBU :: (Statement -> [Statement]) -> [Statement] -> [Statement]
expandStatementsBU f stmts = L.concatMap (expandStatementBU f) stmts

expandStatementBU :: (Statement -> [Statement]) -> Statement -> [Statement]
expandStatementBU f (Loop v s i e body) = f $ Loop v s i e $ expandStatementsBU f body
expandStatementBU f s = f s

applyToLoopBodiesBU :: ([Statement] -> [Statement]) -> [Statement] -> [Statement]
applyToLoopBodiesBU f stmts =
  f $ L.map (applyToLoopBodyBU f) stmts

applyToLoopBodyBU f (Loop v s i e body) = Loop v s i e $ f $ applyToLoopBodiesBU f body
applyToLoopBodyBU f s = s

applyToStatementBU :: (Statement -> Statement) -> Statement -> Statement
applyToStatementBU f (Loop v s i e body) = f $ Loop v s i e $ L.map (applyToStatementBU f) body
applyToStatementBU f s = f s

applyToOperands :: (Matrix -> Matrix) -> Statement -> Statement
applyToOperands f (MatrixAdd c a b) = MatrixAdd (f c) (f a) (f b)
applyToOperands f (ScalarMultiply a alpha b) = ScalarMultiply (f a) (f alpha) (f b)
applyToOperands f (MatrixMultiply c a b) = MatrixMultiply (f c) (f a) (f b)
applyToOperands f (MatrixTranspose a b) = MatrixTranspose (f a) (f b)
applyToOperands f (MatrixSet a b) = MatrixSet (f a) (f b)
applyToOperands f l@(Loop _ _ _ _ _) = l

collectFromAllOperands :: (Matrix -> a) -> Statement -> [a]
collectFromAllOperands f (MatrixMultiply c a b) = [f c, f a, f b]
collectFromAllOperands f (MatrixAdd c a b) = [f c, f a, f b]
collectFromAllOperands f (ScalarMultiply c a b) = [f c, f a, f b]
collectFromAllOperands f (MatrixTranspose a b) = [f a, f b]
collectFromAllOperands f (MatrixSet a b) = [f a, f b]
collectFromAllOperands f (Loop _ _ _ _ body) = L.concatMap (collectFromAllOperands f) body

collectFromStmt :: (Statement -> a) -> Statement -> [a]
collectFromStmt f s@(Loop _ _ _ _ body) = (f s) : (L.concatMap (collectFromStmt f) body)
collectFromStmt f s = [f s]

collectValuesFromStmt :: (Statement -> [a]) -> Statement -> [a]
collectValuesFromStmt f s = L.concat $ collectFromStmt f s

isScalarMultiply (ScalarMultiply _ _ _) = True
isScalarMultiply _ = False

isMatrixSet (MatrixSet _ _) = True
isMatrixSet _ = False

isMatrixAdd (MatrixAdd _ _ _) = True
isMatrixAdd _ = False

isMatrixTranspose (MatrixTranspose _ _) = True
isMatrixTranspose _ = False

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
operandWritten (MatrixTranspose a _) = a
operandWritten (MatrixSet a _) = a

operandsRead (MatrixAdd _ a b) = [a, b]
operandsRead (MatrixMultiply _ a b) = [a, b]
operandsRead (ScalarMultiply _ a b) = [a, b]
operandsRead (MatrixTranspose _ b) = [b]
operandsRead (MatrixSet _ b) = [b]

leftOperand (MatrixAdd _ a _) = a
leftOperand (MatrixMultiply _ a _) = a
leftOperand (ScalarMultiply _ a _) = a

rightOperand (MatrixAdd _ _ b) = b
rightOperand (MatrixMultiply _ _ b) = b
rightOperand (ScalarMultiply _ _ b) = b
rightOperand (MatrixTranspose _ b) = b
rightOperand (MatrixSet _ b) = b

allOperands stmt = L.nub $ allOperandsWithRepeats stmt

allOperandsWithRepeats (MatrixAdd c a b) = [c, a, b]
allOperandsWithRepeats (MatrixMultiply c a b) = [c, a, b]
allOperandsWithRepeats (MatrixTranspose a b) = [a, b]
allOperandsWithRepeats (ScalarMultiply a alpha b) = [a, alpha, b]
allOperandsWithRepeats (MatrixSet a b) = [a, b]
allOperandsWithRepeats (Loop _ _ _ _ _) = []
