module CBackEnd.CodeGeneration.Common(loopToCStmts) where

import Data.List as L

import CBackEnd.Syntax
import IndexExpression
import Statement

loopToCStmts toCStmts l =
  [cFor s e i b ""]
  where
    v = iExprToCExpr $ iVar $ loopInductionVariable l
    s = cAssign v (iExprToCExpr $ loopStart l)
    e = cLEQ v (iExprToCExpr $ loopEnd l)
    i = cAssign v (cAdd v (iExprToCExpr $ loopInc l))
    b = cBlock [] $ L.concatMap toCStmts $ loopBody l
