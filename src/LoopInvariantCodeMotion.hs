module LoopInvariantCodeMotion(pullCodeOutOfLoops) where

import IndexExpression
import Statement

pullCodeOutOfLoops stmts =
  expandStatementsBU pullCodeOutOfLoop stmts

pullCodeOutOfLoop :: Statement -> [Statement]
pullCodeOutOfLoop stmt =
  case isLoop stmt of
    True -> pullLoads stmt
    False -> [stmt]

pullLoads stmt =
  let i = iVar $ loopInductionVariable stmt
      b = loopBody stmt in
  error "pullLoads"
