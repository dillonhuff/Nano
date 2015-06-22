module Statement(Statement,
                 matrixMultiply, matrixAdd, loop,
                 isMatrixAdd, isMatrixMultiply, isLoop,
                 loopStart, loopEnd, loopInc, loopInductionVariable, loopBody,
                 operandWritten, leftOperand, rightOperand,
                 blockMatrixAddM) where

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

blockMAddM indVar blkFactor stmt =
  [mainLoop, residual]
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
    
residualStart blkFactor dimLength =
  let blkC = constVal blkFactor
      dimC = constVal dimLength in
  iConst $ dimC - (mod dimC blkC)

residualLength blkFactor dimLength =
  let blkC = constVal blkFactor
      dimC = constVal dimLength in
  iConst $ mod dimC blkC
