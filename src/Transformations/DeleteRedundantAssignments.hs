module Transformations.DeleteRedundantAssignments(deleteRedundantAssignments) where

import Data.List as L

import Statement

deleteRedundantAssignments :: [Statement] -> [Statement]
deleteRedundantAssignments stmts =
  applyToLoopBodiesBU (L.filter (\stmt -> not $ isRedundantAssignment stmt)) stmts

isRedundantAssignment stmt =
  opcode stmt == MSET && operandWritten stmt == operandRead 0 stmt
  
