module MatrixOperationTests(allMatrixOperationTests) where

import Test.HUnit

import Dummies
import Core.IndexExpression
import Core.Matrix
import Core.MatrixOperation
import Module
import Core.Statement

allMatrixOperationTests = TestLabel "allMatrixOperationTests" $ TestList
  [makeTestCases (linearizeStmts "T_") linearizeStmtsCases]

linearizeStmtsCases =
  [([dmasg (dmat a) (dmat b)], [matrixSet a b]),
   ([dmasg (dmat a) (dmBinop MAdd (dmat b) (dmat c))],
     [matrixAdd tempB0 b c, matrixSet a tempB0]),
   ([dmasg (dmat a) (dmBinop SMul (dmat alpha) (dmat a))],
     [scalarMultiply tempA0 alpha a, matrixSet a tempA0]),
   ([dmasg (dmat c) (dmBinop MMul (dmat a) (dmat b))],
     [setZero tempC0, matrixMultiply tempC0 a b, matrixSet c tempC0]),
   ([dmasg (dmat b) (dmUnop MTrans (dmat a))],
     [matrixTranspose tempB0T a, matrixSet b tempB0T])]

tempA0 = setName "T_0" $ setLocal a
tempB0 = setName "T_0" $ setLocal b
tempB0T = setName "T_0" $ setLocal $
       matrix (bufferName b) (numCols a) (numRows a) (iConst 1) (numCols a) (matProperties a)
tempC0 = setName "T_0" $ setLocal $
         matrix (bufferName a) (numRows a) (numCols b) (iConst 1) (numRows a) (matProperties a)
