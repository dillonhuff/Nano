module RegisterizeTemps(registerizeTemps) where

import Data.List as L

import Analysis.Matrix
import Matrix
import Statement

registerizeTemps u stmts =
  let allUnderlyingMats = L.nub $ L.concatMap (collectFromAllOperands underlyingMatrix) stmts
      allUnderlyingTempMats = L.filter (\m -> bufferScope m == local) allUnderlyingMats in
  L.foldr (tryToRegisterize u) stmts allUnderlyingTempMats

tryToRegisterize :: Int -> Matrix -> [Statement] -> [Statement]
tryToRegisterize u m stmts =
  case isRegisterizeable u m of
    True -> expandStatementsBU (\st -> [applyToOperands (replaceSupermatrix m (setRegister m)) st]) stmts
    False -> stmts
