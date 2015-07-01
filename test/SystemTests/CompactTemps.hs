module SystemTests.CompactTemps(allCompactTempsTests) where

import Data.List as L
import Test.HUnit

import Blocking
import CompactTemps
import Dummies
import Fusion
import Fuzz
import IndexExpression
import Scalarization
import Statement

allCompactTempsTests = TestLabel "All scalarization system tests" $
                      TestList $ L.map (\op -> TestCase $ assertOptimizationsCorrect compactTempsOpts op) compoundTestOperations

compactTempsOpts = (scalarize "r_"):compactTemps:preprocessingOpts ++ [fuseInnerLoops]

preprocessingOpts =
  L.intersperse fuseInnerLoops $ 
  L.map (\t -> expandStatementsBU t)
  [blockMatrixMultiplyP (iVar "i3") (iConst 1),
   
   blockMatrixAddM (iVar "i4") (iConst 1),
   blockMatrixMultiplyM (iVar "i1") (iConst 1),
   blockScalarMultiplyM (iVar "i6") (iConst 1),
   blockMatrixTransposeM (iVar "i8") (iConst 1),

   blockMatrixAddN (iVar "i5") (iConst 1),
   blockMatrixMultiplyN (iVar "i2") (iConst 1),
   blockScalarMultiplyN (iVar "i7") (iConst 1),
   blockMatrixTransposeN (iVar "i9") (iConst 1)]


   
