module InterchangeAndFuse(interchangeAndFuse) where

import Fusion
import Statement
import StatementInterchange

interchangeAndFuse stmts = applyToLoopBodiesBU (tryToInterchangeAndFuse $ iVarRanges stmts) stmts

tryToInterchangeAndFuse iRanges (l1:r:l2:rest) =
  case canFuseIfAdjacent l1 l2 && canInterchange iRanges r l2 of
    True -> (fuseLoops l1 l2):(tryToInterchangeAndFuse iRanges (r:rest))
    False -> l1 : (tryToInterchangeAndFuse iRanges (r:l2:rest))
tryToInterchangeAndFuse iRanges other = other
