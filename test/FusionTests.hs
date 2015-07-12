module FusionTests(allFusionTests) where

import Data.List as L
import Test.HUnit

import Transformations.Blocking
import Dummies
import Transformations.Fusion
import IndexExpression
import Module
import Statement

allFusionTests = TestLabel "All fusion tests" $ TestList
               [makeTestCases canFuseTest cantFuseCases,
                makeTestCases canFuseTest canFuseCases]

cantFuseCases =
  L.map (\(x, y) -> ((x, y), False))
  [(matrixAdd a b c, matrixAdd a b c),
   (maddAAABlockM3, maddCCCBlockM4),
   (maddAAABlockM3, maddXXXBlockM3),
   (matMulAlphaPXBlockP5, smulZAlphaZBlockM5)]

canFuseCases =
  L.map (\(x, y) -> ((x, y), True))
  [(maddAAABlockM3, maddCCCBlockM3),
   (maddABBBlockN4, maddCAABlockN4),
   (maddCCCBlockM4, maddABBBlockN4),
   (smulAAlphaABlockM4, mmulCABBlockM4)]

canFuseTest (left, right) =
  canFuseIfAdjacent left right

maddAAABlockM3 = L.head $ blockMatrixAddM (iVar "i") (iConst 3) $ matrixAdd a a a

maddCCCBlockM3 = L.head $ blockMatrixAddM (iVar "i") (iConst 3) $ matrixAdd c c c
maddCCCBlockM4 = L.head $ blockMatrixAddM (iVar "i") (iConst 4) $ matrixAdd c c c

maddXXXBlockM3 = L.head $ blockMatrixAddM (iVar "j") (iConst 3) $ matrixAdd x x x

matMulAlphaPXBlockP5 = L.head $ blockMatrixMultiplyP (iVar "b") (iConst 5) $ matrixMultiply alpha p x

smulZAlphaZBlockM5 = L.head $ blockScalarMultiplyM (iVar "k") (iConst 5) $ scalarMultiply z alpha z

smulAAlphaABlockM4 = L.head $ blockScalarMultiplyM (iVar "k2") (iConst 4) $ scalarMultiply a alpha a

maddABBBlockN4 = L.head $ blockMatrixAddN (iVar "i1") (iConst 4) $ matrixAdd a b b

maddCAABlockN4 = L.head $ blockMatrixAddN (iVar "i2") (iConst 4) $ matrixAdd c a a

mmulCABBlockM4 = L.head $ blockMatrixMultiplyM (iVar "i3") (iConst 4) $ matrixMultiply c a b
