module Core.Statement(Statement,
                 matrixMultiply, matrixTranspose, matrixAdd, loop, scalarMultiply, matrixSet,
                 matrixPack, matrixUnpack, fusedMultiplyAdd,
                 broadcast, elemWiseMultiply, setZero, accumulate,
                 isMatrixAdd, isMatrixTranspose, isMatrixMultiply, isLoop, isScalarMultiply, isMatrixSet,
                 loopStart, loopEnd, loopInc, loopInductionVariable, loopBody, opcode,
                 operandWritten, allOperands, operandsRead,
                 expandStatementBU, expandStatementsBU, expandStatementBUM, expandStatementsBUM,
                 applyToOperands, applyToStatementBU, applyToLoopBodiesBU,
                 collectFromAllOperands,
                 collectFromStmt, collectValuesFromStmt,
                 operandRead,
                 anyNullOperands,
                 setOperandWritten, setOperandsRead,
                 OpCode(..)) where

import Control.Monad
import Data.List as L

import Core.IndexExpression
import Core.Matrix

data Statement
  = Instr OpCode Matrix [Matrix]
  | Loop String IExpr IExpr IExpr [Statement]
    deriving (Eq, Ord)

instance Show Statement where
  show (Instr other w args) = "\n" ++ show other ++ " " ++ show w ++ " " ++ show args
  show (Loop v s b e body) = "for " ++ v ++ " " ++ show s ++ ":" ++ show b ++ ":" ++ show e ++ " " ++ (L.concatMap show body) ++ "\nend"

fusedMultiplyAdd c a b = Instr FMA c [a, b, c]
accumulate c a b = Instr ACCU c [a, b]
setZero a = Instr ZERO a []
broadcast a b = Instr BRDC a [b]
elemWiseMultiply c a b = Instr EMUL c [a, b]
matrixPack a b = Instr PACK a [b]
matrixUnpack a b = Instr UNPK a [b]
matrixSet a b =
  case (isRegister a && isRegister b) ||
       (not (isRegister a) && not (isRegister b)) of
    True -> Instr MSET a [b]
    False -> error $ "matrixSet: Operands " ++ show a ++ "\nand\n" ++ show b ++ " are not both registers or both memory"
matrixAdd c a b = Instr EADD c [a, b]
matrixMultiply c a b = Instr MMUL c [a, b, c]
matrixTranspose a b = Instr TRAN a [b]
scalarMultiply c a b = Instr SMUL c [a, b]
loop = Loop

opcode (Instr c _ _) = c
opcode (Loop _ _ _ _ _) = LOOP

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
applyToOperands f (Instr c w args) = Instr c (f w) $ L.map f args
applyToOperands f l@(Loop _ _ _ _ _) = l

collectFromAllOperands :: (Matrix -> a) -> Statement -> [a]
collectFromAllOperands f (Loop _ _ _ _ body) = L.concatMap (collectFromAllOperands f) body
collectFromAllOperands f i = L.map f $ allOperands i

collectFromStmt :: (Statement -> a) -> Statement -> [a]
collectFromStmt f s@(Loop _ _ _ _ body) = (f s) : (L.concatMap (collectFromStmt f) body)
collectFromStmt f s = [f s]

collectValuesFromStmt :: (Statement -> [a]) -> Statement -> [a]
collectValuesFromStmt f s = L.concat $ collectFromStmt f s

isMatrixSet i = opcode i == MSET
isMatrixAdd i = opcode i == EADD
isScalarMultiply i = opcode i == SMUL
isMatrixTranspose i = opcode i == TRAN
isMatrixMultiply i = opcode i == MMUL

isLoop (Loop _ _ _ _ _) = True
isLoop _ = False

loopStart (Loop _ s _ _ _) = s
loopEnd (Loop _ _ _ e _) = e
loopInc (Loop _ _ i _ _) = i
loopInductionVariable (Loop v _ _ _ _) = v
loopBody (Loop _ _ _ _ b) = b

operandWritten (Instr _ w _) = w

operandsRead (Instr _ _ args) = args

operandRead n (Instr _ _ args) = args !! n

allOperands stmt = L.nub $ allOperandsWithRepeats stmt

allOperandsWithRepeats (Instr _ w args) = w:args
allOperandsWithRepeats (Loop _ _ _ _ _) = []

anyNullOperands stmt =
  L.any isNull $ allOperands stmt

setOperandWritten w (Instr c _ args) = Instr c w args

setOperandsRead args (Instr c w _) = Instr c w args

data OpCode
  = MMUL
  | SMUL
  | EMUL
  | EADD
  | MSET
  | TRAN
  | LOOP
  | BRDC
  | ZERO
  | ACCU
  | PACK
  | UNPK
  | FMA
    deriving (Eq, Ord, Show)
