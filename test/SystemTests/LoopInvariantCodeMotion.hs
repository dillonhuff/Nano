module SystemTests.LoopInvariantCodeMotion(allLoopInvariantCodeMotionTests) where

import Test.HUnit

import CBackEnd.CodeGeneration.Common
import CBackEnd.CodeGeneration.Scalar
import Core.IndexExpression
import Core.Statement
import Dummies
import Fuzz
import TestUtils
import Transformations.Blocking
import Transformations.CompactTemps
import Transformations.Fusion
import Transformations.LoopInvariantCodeMotion
import Transformations.IntroducePacking
import Transformations.ScalarMMULToFMA

allLoopInvariantCodeMotionTests =
  TestLabel "All loop invariant code motion system tests" $
  TestList $ licmTests

licmTests =
  [ltc "scalar multiply vector" stmtsToScalarC licmTempsOpts [scalarMultiply x alpha x],
   ltc "daxpy like operation" stmtsToScalarC licmTempsOpts [scalarMultiply x alpha x, matrixAdd y x y],
   ltc "scalar multiply matrix" stmtsToScalarC licmTempsOpts [scalarMultiply c alpha c],
   ltc "matrix add" stmtsToScalarC licmTempsOpts [matrixAdd a b c],
   ltc "scalar multiply then matrix add" stmtsToScalarC licmTempsOpts [scalarMultiply tr9c9 alpha a, matrixAdd a tr9c9 tr9c9],
   ltc "scalar multiply then matrix multiply" stmtsToScalarC licmMMulOpts [scalarMultiply tr9c9 alpha a, matrixMultiply c tr9c9 b],
   ltc "dotmul" stmtsToScalarC licmMMulOpts [matrixMultiply alpha p x, scalarMultiply y alpha y]]

licmTempsOpts = pullCodeOutOfLoops:(pack 1 "r_"):compactTemps:(scalarMMULToFMA "fma"):preprocessingOpts
licmMMulOpts = pullCodeOutOfLoops:(pack 1 "r_"):compactTemps:(scalarMMULToFMA "fma"):preprocessMMulOpts
