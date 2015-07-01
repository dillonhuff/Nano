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
                      TestList $ compactTests

compactTests =
  [TestCase $ assertOptimizationsCorrect compactTempsOpts [matrixAdd a b c],
   TestCase $ assertOptimizationsCorrect compactTempsOpts [matrixAdd tr9c9 b b, matrixAdd a tr9c9 tr9c9],
   TestCase $ assertOptimizationsCorrect compactTempsOpts [matrixAdd tr9c9 a a, matrixAdd a b tr9c9],
   TestCase $ assertOptimizationsCorrect compactTempsOpts [scalarMultiply tr9c9 alpha a, matrixAdd a tr9c9 tr9c9],
   TestCase $ assertOptimizationsCorrect compactMMulOpts [scalarMultiply tr9c9 alpha a, matrixMultiply c tr9c9 b],
   TestCase $ assertOptimizationsCorrect compactTransOpts [matrixTranspose tr9c9 a, matrixAdd c b tr9c9]]

compactTempsOpts = (scalarize "r_"):compactTemps:preprocessingOpts
compactMMulOpts = (scalarize "r_"):compactTemps:preprocessMMulOpts
compactTransOpts = (scalarize "r_"):compactTemps:fuseInnerLoops:preprocessTransOpts

preprocessingOpts =
  L.intersperse fuseInnerLoops $ 
  L.map (\t -> expandStatementsBU t)
  [blockMatrixMultiplyP (iVar "i3") (iConst 1),
   

   blockMatrixAddN (iVar "i5") (iConst 1),
   blockMatrixMultiplyN (iVar "i2") (iConst 1),
   blockScalarMultiplyN (iVar "i7") (iConst 1),
   blockMatrixTransposeN (iVar "i9") (iConst 1),

   blockMatrixAddM (iVar "i4") (iConst 1),
   blockMatrixMultiplyM (iVar "i1") (iConst 1),
   blockScalarMultiplyM (iVar "i6") (iConst 1),
   blockMatrixTransposeM (iVar "i8") (iConst 1)]


preprocessMMulOpts =
  L.intersperse fuseInnerLoops $
  L.map (\t -> expandStatementsBU t)
  [blockMatrixMultiplyN (iVar "i7") (iConst 1),
   
   blockMatrixMultiplyP (iVar "i4") (iConst 1),
   blockScalarMultiplyN (iVar "i3") (iConst 1),
   
   blockMatrixMultiplyM (iVar "i2") (iConst 1),
   blockScalarMultiplyM (iVar "i1") (iConst 1)]

preprocessTransOpts =
  L.intersperse fuseInnerLoops $
  L.map (\t -> expandStatementsBU t)
  [blockMatrixTransposeN (iVar "i1") (iConst 1),
   blockMatrixAddN (iVar "i2") (iConst 1),
   
   blockMatrixTransposeM (iVar "i3") (iConst 1),
   blockMatrixAddM (iVar "i4") (iConst 1)]
