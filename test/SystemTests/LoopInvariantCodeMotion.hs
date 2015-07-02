module SystemTests.LoopInvariantCodeMotion(allLoopInvariantCodeMotionTests) where

import Test.HUnit

import Blocking
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
  [ltc "matrix add" licmTempsOpts [matrixAdd a b c],
   ltc "scalar multiply then matrix add" licmTempsOpts [scalarMultiply tr9c9 alpha a, matrixAdd a tr9c9 tr9c9],
   ltc "scalar multiply then matrix multiply" licmMMulOpts [scalarMultiply tr9c9 alpha a, matrixMultiply c tr9c9 b]]

licmTempsOpts = (scalarize "r_"):pullCodeOutOfLoops:compactTemps:preprocessingOpts
licmMMulOpts = (scalarize "r_"):pullCodeOutOfLoops:compactTemps:preprocessMMulOpts

