module Transformations.RegisterizeTemps(registerizeTemps) where

import Data.List as L

import Analysis.Matrix
import Core.IndexExpression
import Core.Matrix
import Core.Statement
import Utils

registerizeTemps u stmts =
  registerizeTempsWith u (\m -> isRegisterizeable u m || isRegisterizeableBelow u m) stmts
  
registerizeTempsWith u f stmts =
  let allUnderlyingMats = L.nub $ L.concatMap (collectFromAllOperands underlyingMatrix) stmts
      allUnderlyingTempMats = L.filter (\m -> bufferScope m == local) allUnderlyingMats in
  L.foldr (tryToRegisterizeWith (iConst u) f) stmts allUnderlyingTempMats
  
tryToRegisterizeWith :: IExpr -> (Matrix -> Bool) -> Matrix -> [Statement] -> [Statement]
tryToRegisterizeWith u registerizeableCondition m stmts =
  case registerizeableCondition m of
    True -> expandStatementsBU (\st -> registerizeTempInStmt u m st) stmts
    False -> stmts

registerizeTempInStmt u m stmt =
  case (opcode stmt == PACK && operandRead 0 stmt == m) || (opcode stmt == UNPK && operandWritten stmt == m) of
    True -> [replacePKUNPKWithSet u m stmt]
    False -> [applyToOperands (replaceSupermatrix m (mkRegister u m)) stmt]

replacePKUNPKWithSet u m stmt =
  let newWritten = replaceSupermatrix m (mkRegister u m) (operandWritten stmt)
      newRead = replaceSupermatrix m (mkRegister u m) (operandRead 0 stmt) in
  matrixSet newWritten newRead
