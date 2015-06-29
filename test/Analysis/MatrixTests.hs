module Analysis.MatrixTests(allMatrixTests) where

import Test.HUnit

import Analysis.Matrix
import Dummies
import IndexExpression
import Matrix
import Module

allMatrixTests =
  TestLabel "All matrix analysis tests" $ TestList
  [makeTestCases matricesOverlapTest matricesOverlapCases]

matricesOverlapTest (s, t) =
  matricesOverlap dummyRanges s t

matricesOverlapCases =
  [((a, a), True),
   ((a, b), False),
   ((rowPart (iVar "k") (iConst 1) a, rowPart (iConst 5) (iConst 1) a), False)]
