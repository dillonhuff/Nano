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
  [ltc "scalar multiply vector" scalarVarDecls stmtsToScalarC licmTempsOpts [scalarMultiply x alpha x],
   ltc "daxpy like operation" scalarVarDecls stmtsToScalarC licmTempsOpts [scalarMultiply x alpha x, matrixAdd y x y],
   ltc "scalar multiply matrix" scalarVarDecls stmtsToScalarC licmTempsOpts [scalarMultiply c alpha c],
   ltc "matrix add" scalarVarDecls stmtsToScalarC licmTempsOpts [matrixAdd a b c],
   ltc "scalar multiply then matrix add" scalarVarDecls stmtsToScalarC licmTempsOpts [scalarMultiply tr9c9 alpha a, matrixAdd a tr9c9 tr9c9],
   ltc "scalar multiply then matrix multiply" scalarVarDecls stmtsToScalarC licmMMulOpts [scalarMultiply tr9c9 alpha a, matrixMultiply c tr9c9 b],
   ltc "dotmul" scalarVarDecls stmtsToScalarC licmMMulOpts [matrixMultiply alpha p x, scalarMultiply y alpha y]]

licmTempsOpts = pullCodeOutOfLoops:(pack 1 "r_"):compactTemps:preprocessingOpts
licmMMulOpts = pullCodeOutOfLoops:(pack 1 "r_"):compactTemps:preprocessMMulOpts
