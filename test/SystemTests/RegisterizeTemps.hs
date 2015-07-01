module SystemTests.RegisterizeTemps(allRegisterizeTempsTests) where

import Data.List as L
import Test.HUnit

import Blocking
import CompactTemps
import Dummies
import Fusion
import Fuzz
import IndexExpression
import RegisterizeTemps
import Scalarization
import Statement

allRegisterizeTempsTests = TestLabel "All scalarization system tests" $
                      TestList $ registerizeTests

registerizeTests =
  [TestCase $ assertOptimizationsCorrect registerizeTempsOpts [matrixAdd a b c],
   TestCase $ assertOptimizationsCorrect registerizeTempsOpts [matrixAdd tr9c9 b b, matrixAdd a tr9c9 tr9c9],
   TestCase $ assertOptimizationsCorrect registerizeTempsOpts [matrixAdd tr9c9 a a, matrixAdd a b tr9c9],
   TestCase $ assertOptimizationsCorrect registerizeTempsOpts [scalarMultiply tr9c9 alpha a, matrixAdd a tr9c9 tr9c9],
   TestCase $ assertOptimizationsCorrect registerizeMMulOpts [scalarMultiply tr9c9 alpha a, matrixMultiply c tr9c9 b],
   TestCase $ assertOptimizationsCorrect registerizeTransOpts [matrixTranspose tr9c9 a, matrixAdd c b tr9c9]]

registerizeTempsOpts = (scalarize "r_"):registerizeTemps:compactTemps:preprocessingOpts
registerizeMMulOpts = (scalarize "r_"):registerizeTemps:compactTemps:preprocessMMulOpts
registerizeTransOpts = (scalarize "r_"):registerizeTemps:compactTemps:fuseInnerLoops:preprocessTransOpts

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
