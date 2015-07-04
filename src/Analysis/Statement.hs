module Analysis.Statement(isScalarOp,
                          isScalarOpBelow) where

import Data.List as L

import Analysis.Matrix
import Statement

isScalarOp :: Int -> Statement -> Bool
isScalarOp u stmt = L.all (isRegisterizeable u) $ allOperands stmt

isScalarOpBelow :: Int -> Statement -> Bool
isScalarOpBelow u stmt = L.all (isRegisterizeableBelow u) $ allOperands stmt
