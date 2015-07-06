module RegisterizeTemps(registerizeTemps,
                        registerizeTempsBelow) where

import Data.List as L

import Analysis.Matrix
import IndexExpression
import Matrix
import Statement
import Utils

registerizeTemps u stmts =
  registerizeTempsWith u (isRegisterizeable u) stmts
  
registerizeTempsBelow u stmts =
  registerizeTempsWith u (isRegisterizeableBelow u) stmts

registerizeTempsWith u f stmts =
  let allUnderlyingMats = L.nub $ L.concatMap (collectFromAllOperands underlyingMatrix) stmts
      allUnderlyingTempMats = L.filter (\m -> bufferScope m == local) allUnderlyingMats in
  L.foldr (tryToRegisterizeWith (iConst u) f) stmts allUnderlyingTempMats
  
tryToRegisterizeWith :: IExpr -> (Matrix -> Bool) -> Matrix -> [Statement] -> [Statement]
tryToRegisterizeWith u registerizeableCondition m stmts =
  case registerizeableCondition m of
    True -> expandStatementsBU (\st -> [applyToOperands (replaceSupermatrix m (mkRegister u m)) st]) stmts
    False -> stmts
