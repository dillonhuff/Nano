module RegisterizeTemps(registerizeTemps,
                        registerizeTempsBelow) where

import Data.List as L

import Analysis.Matrix
import Matrix
import Statement


registerizeTemps u stmts =
  registerizeTempsWith (isRegisterizeable u) stmts
  
registerizeTempsBelow u stmts =
  registerizeTempsWith (isRegisterizeableBelow u) stmts

registerizeTempsWith f stmts =
  let allUnderlyingMats = L.nub $ L.concatMap (collectFromAllOperands underlyingMatrix) stmts
      allUnderlyingTempMats = L.filter (\m -> bufferScope m == local) allUnderlyingMats in
  L.foldr (tryToRegisterizeWith f) stmts allUnderlyingTempMats
  
tryToRegisterizeWith :: (Matrix -> Bool) -> Matrix -> [Statement] -> [Statement]
tryToRegisterizeWith registerizeableCondition m stmts =
  case registerizeableCondition m of
    True -> expandStatementsBU (\st -> [applyToOperands (replaceSupermatrix m (setRegister m)) st]) stmts
    False -> stmts
