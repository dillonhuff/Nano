module SystemTests.LoopInvariantCodeMotion(allLoopInvariantCodeMotionTests) where

import Test.HUnit

import Transformations.Blocking
import CBackEnd.CodeGeneration.Common
import CBackEnd.CodeGeneration.Scalar
import Transformations.CompactTemps
import Dummies
import Transformations.Fusion
import Fuzz
import Core.IndexExpression
import Transformations.LoopInvariantCodeMotion
import Transformations.IntroducePacking
import Core.Statement
import TestUtils

allLoopInvariantCodeMotionTests =
  TestLabel "All loop invariant code motion system tests" $
  TestList $ licmTests

licmTests =
  [ltc "scalar multiply vector" scalarVarDecls toScalarC licmTempsOpts [scalarMultiply x alpha x],
   ltc "daxpy like operation" scalarVarDecls toScalarC licmTempsOpts [scalarMultiply x alpha x, matrixAdd y x y],
   ltc "scalar multiply matrix" scalarVarDecls toScalarC licmTempsOpts [scalarMultiply c alpha c],
   ltc "matrix add" scalarVarDecls toScalarC licmTempsOpts [matrixAdd a b c],
   ltc "scalar multiply then matrix add" scalarVarDecls toScalarC licmTempsOpts [scalarMultiply tr9c9 alpha a, matrixAdd a tr9c9 tr9c9],
   ltc "scalar multiply then matrix multiply" scalarVarDecls toScalarC licmMMulOpts [scalarMultiply tr9c9 alpha a, matrixMultiply c tr9c9 b],
   ltc "dotmul" scalarVarDecls toScalarC licmMMulOpts [matrixMultiply alpha p x, scalarMultiply y alpha y]]

licmTempsOpts = pullCodeOutOfLoops:(pack 1 "r_"):compactTemps:preprocessingOpts
licmMMulOpts = pullCodeOutOfLoops:(pack 1 "r_"):compactTemps:preprocessMMulOpts
