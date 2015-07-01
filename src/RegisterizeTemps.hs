module RegisterizeTemps(registerizeTemps) where

import Data.List as L

import Matrix
import Statement

registerizeTemps stmts =
  let allUnderlyingMats = L.nub $ L.concatMap (collectFromAllOperands underlyingMatrix) stmts
      allUnderlyingTempMats = L.filter (\m -> bufferScope m == local) allUnderlyingMats in
  L.foldr tryToRegisterize stmts allUnderlyingTempMats

tryToRegisterize :: Matrix -> [Statement] -> [Statement]
tryToRegisterize m stmts =
  case isScalar m of
    True -> expandStatementsBU (\st -> [applyToOperands (replaceSupermatrix m (setRegister m)) st]) stmts
    False -> stmts
