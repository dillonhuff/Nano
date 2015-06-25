module FusionTests(allFusionTests) where

import Data.List as L
import Test.HUnit

import Blocking
import Dummies
import Fusion
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
  [(maddAAABlockM3, maddCCCBlockM3)]


canFuseTest (left, right) =
  canFuseIfAdjacent left right

maddAAABlockM3 = L.head $ blockMatrixAddM (iVar "i") (iConst 3) $ matrixAdd a a a

maddCCCBlockM3 = L.head $ blockMatrixAddM (iVar "i") (iConst 3) $ matrixAdd c c c
maddCCCBlockM4 = L.head $ blockMatrixAddM (iVar "i") (iConst 4) $ matrixAdd c c c

maddXXXBlockM3 = L.head $ blockMatrixAddM (iVar "j") (iConst 3) $ matrixAdd x x x

matMulAlphaPXBlockP5 = L.head $ blockMatrixMultiplyP (iVar "b") (iConst 5) $ matrixMultiply alpha p x

smulZAlphaZBlockM5 = L.head $ blockScalarMultiplyM (iVar "k") (iConst 5) $ scalarMultiply z alpha z

