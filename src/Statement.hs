module Statement(Statement,
                 matrixMultiply, matrixAdd, loop,
                 isMatrixAdd, isMatrixMultiply, isLoop,
                 loopStart, loopEnd, loopInc, loopInductionVariable, loopBody,
                 operandWritten, leftOperand, rightOperand,
                 blockMatrixAddM, blockMatrixAddN,
                 collectFromAllOperands,
                 collectFromStmt, collectValuesFromStmt) where

import Data.List as L

import IndexExpression
import Matrix

data Statement
  = MatrixMultiply Matrix Matrix Matrix
  | MatrixAdd Matrix Matrix Matrix
  | Loop String IExpr IExpr IExpr [Statement]
    deriving (Eq, Ord, Show)

matrixMultiply = MatrixMultiply
matrixAdd = MatrixAdd
loop = Loop

collectFromAllOperands :: (Matrix -> a) -> Statement -> [a]
collectFromAllOperands f (MatrixMultiply c a b) = [f c, f a, f b]
collectFromAllOperands f (MatrixAdd c a b) = [f c, f a, f b]
collectFromAllOperands _ _ = []

collectFromStmt :: (Statement -> a) -> Statement -> [a]
collectFromStmt f s@(Loop _ _ _ _ body) = (f s) : (L.concatMap (collectFromStmt f) body)
collectFromStmt f s = [f s]

collectValuesFromStmt :: (Statement -> [a]) -> Statement -> [a]
collectValuesFromStmt f s = L.concat $ collectFromStmt f s

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
leftOperand (MatrixAdd _ a _) = a
rightOperand (MatrixAdd _ _ b) = b

blockMatrixAddM indVar blockFactor stmt =
  case isMatrixAdd stmt of
    True -> blockMAddM indVar blockFactor stmt
    False -> [stmt]


blockMatrixAddN indVar blkFactor stmt =
  case isMatrixAdd stmt of
    True -> blockMAddN indVar blkFactor stmt
    False -> [stmt]

blockMAddM indVar blkFactor stmt =
  case numRows resC == iConst 0 of
    True -> [mainLoop]
    False -> [mainLoop, residual]
  where
    c = operandWritten stmt
    a = leftOperand stmt
    b = rightOperand stmt
    mainC = subMatrix indVar blkFactor (iConst 0) (numCols c) c
    mainA = subMatrix indVar blkFactor (iConst 0) (numCols a) a
    mainB = subMatrix indVar blkFactor (iConst 0) (numCols b) b
    rs = residualStart blkFactor (numRows c)
    rl = residualLength blkFactor (numRows c)
    resC = subMatrix rs rl (iConst 0) (numCols c) c
    resB = subMatrix rs rl (iConst 0) (numCols b) b
    resA = subMatrix rs rl (iConst 0) (numCols a) a
    e = evaluateIExprConstants $ iSub (numRows c) blkFactor
    mainAdd = matrixAdd mainC mainA mainB
    mainLoop = loop (varName indVar) (iConst 0) blkFactor e [mainAdd]
    residual = matrixAdd resC resA resB

blockMAddN indVar blkFactor stmt =
  case numCols resC == iConst 0 of
    True -> [mainLoop]
    False -> [mainLoop, residual]
  where
    c = operandWritten stmt
    a = leftOperand stmt
    b = rightOperand stmt
    mainC = subMatrix (iConst 0) (numRows c) indVar blkFactor c
    mainA = subMatrix (iConst 0) (numRows a) indVar blkFactor a
    mainB = subMatrix (iConst 0) (numRows b) indVar blkFactor b
    rs = residualStart blkFactor (numRows c)
    rl = residualLength blkFactor (numRows c)
    resC = subMatrix (iConst 0) (numCols c) rs rl c
    resB = subMatrix (iConst 0) (numCols b) rs rl b
    resA = subMatrix (iConst 0) (numCols a) rs rl a
    e = evaluateIExprConstants $ iSub (numCols c) blkFactor
    mainAdd = matrixAdd mainC mainA mainB
    mainLoop = loop (varName indVar) (iConst 0) blkFactor e [mainAdd]
    residual = matrixAdd resC resA resB
  
residualStart blkFactor dimLength =
  let blkC = constVal blkFactor
      dimC = constVal dimLength in
  iConst $ dimC - (mod dimC blkC)

residualLength blkFactor dimLength =
  let blkC = constVal blkFactor
      dimC = constVal dimLength in
  iConst $ mod dimC blkC

