module MatrixTests(allMatrixTests) where

import Data.List as L
import Test.HUnit

import Analysis.IndexExpression
import IndexExpression
import Dummies
import Matrix
import Module

allMatrixTests = TestLabel "All matrix tests" $ TestList
  [makeTestCases numRows numRowsCases,
   makeTestCases numCols numColsCases,
   makeTestCases (accessedRectangle dummyRanges) accessedRectangleCases]

numRowsCases =
  L.map (\(x, y) -> (x, iConst y))
  [(m, 17),
   (rowPart (iVar "i") (iConst 4) m, 4),
   (rowPart (iConst 4) (iConst 2) m, 2)]

numColsCases =
  L.map (\(x, y) -> (x, iConst y))
  [(m, 8),
   (colPart (iVar "j") (iConst 2) m, 2)]

accessedRectangleCases =
  [(m, Just $ constRect 0 16 0 7),
   (rowPart (iConst 3) (iConst 2) m, Just $ constRect 3 4 0 7),
   (colPart (iConst 5) (iConst 3) m, Just $ constRect 0 16 5 7),
   (rowPart (iVar "i") (iConst 2) m, Just $ constRect 0 15 0 7)]

m = matrix "A" (iConst 17) (iConst 8) (iConst 1) (iConst 17) (properties arg double memory)
