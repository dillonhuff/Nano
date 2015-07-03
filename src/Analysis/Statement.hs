module Analysis.Statement(isScalarOp) where

import Data.List as L

import Analysis.Matrix
import Statement

isScalarOp :: Int -> Statement -> Bool
isScalarOp u stmt = L.all (isRegisterizeable u) $ allOperands stmt
