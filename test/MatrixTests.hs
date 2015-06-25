module MatrixTests(allMatrixTests) where

import Data.List as L
import Test.HUnit

import IndexExpression
import Matrix
import Module

allMatrixTests = TestLabel "All matrix tests" $ TestList
  [makeTestCases numRows numRowsCases,
   makeTestCases numCols numColsCases]

numRowsCases =
  L.map (\(x, y) -> (x, iConst y))
  [(a, 17),
   (subMatrix (iVar "i") (iConst 4) (iConst 0) (iConst 4) a, 4),
   (subMatrix (iConst 4) (iConst 2) (iVar "j") (iConst 1) a, 2)]

numColsCases =
  L.map (\(x, y) -> (x, iConst y))
  [(a, 8),
   (subMatrix (iVar "j") (iConst 2) (iConst 0) (iConst 4) a, 4)]

a = matrix "A" (iConst 17) (iConst 8) (iConst 1) (iConst 17) (properties arg double)
