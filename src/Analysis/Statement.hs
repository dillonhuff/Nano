module Analysis.Statement(isScalarOp,
                          isScalarOpBelow,
                          isRegisterGroupOp,
                          allOperandsInMemory) where

import Data.List as L

import Analysis.Matrix
import Core.Matrix
import Core.Statement

isScalarOp :: Int -> Statement -> Bool
isScalarOp u stmt = L.all (isRegisterizeable u) $ allOperands stmt

isScalarOpBelow :: Int -> Statement -> Bool
isScalarOpBelow u stmt = L.all (isRegisterizeableBelow u) $ allOperands stmt

allOperandsInMemory :: Statement -> Bool
allOperandsInMemory stmt = L.all (\m -> not $ isRegister m) $ allOperands stmt

isRegisterGroupOp :: Int -> Int -> Statement -> Bool
isRegisterGroupOp m n stmt = L.all (isLEQ m n) $ allOperands stmt
