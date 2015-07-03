module SystemTests.LoopInvariantCodeMotion(allLoopInvariantCodeMotionTests) where

import Test.HUnit

import Blocking
import CBackEnd.CodeGeneration.Scalar
import CompactTemps
import Dummies
import Fusion
import Fuzz
import IndexExpression
import LoopInvariantCodeMotion
import Scalarization
import Statement
import TestUtils

allLoopInvariantCodeMotionTests =
  TestLabel "All loop invariant code motion system tests" $
  TestList $ licmTests

licmTests =
  [ltc "scalar multiply vector" toScalarC licmTempsOpts [scalarMultiply x alpha x],
   ltc "daxpy like operation" toScalarC licmTempsOpts [scalarMultiply x alpha x, matrixAdd y x y],
   ltc "scalar multiply matrix" toScalarC licmTempsOpts [scalarMultiply c alpha c],
   ltc "matrix add" toScalarC licmTempsOpts [matrixAdd a b c],
   ltc "scalar multiply then matrix add" toScalarC licmTempsOpts [scalarMultiply tr9c9 alpha a, matrixAdd a tr9c9 tr9c9],
   ltc "scalar multiply then matrix multiply" toScalarC licmMMulOpts [scalarMultiply tr9c9 alpha a, matrixMultiply c tr9c9 b],
   ltc "dotmul" toScalarC licmMMulOpts [matrixMultiply alpha p x, scalarMultiply y alpha y]]

licmTempsOpts = pullCodeOutOfLoops:(scalarize "r_"):compactTemps:preprocessingOpts
licmMMulOpts = pullCodeOutOfLoops:(scalarize "r_"):compactTemps:preprocessMMulOpts
