module SystemTests.AllSystemTests(allSystemTests) where

import Test.HUnit

import SystemTests.AVXCodeGen
import SystemTests.BlockingTests
import SystemTests.CompactTemps
import SystemTests.Fusion
import SystemTests.InterchangeAndFuse
import SystemTests.LoopInvariantCodeMotion
import SystemTests.Lv2BlockingSearch
import SystemTests.MultiBlocking
import SystemTests.RegisterizeTemps
import SystemTests.Scalarization
import SystemTests.SplitTemps

allSystemTests = TestLabel "All sytem tests" $ TestList
               [allAVXCodeGenTests,
                allCompactTempsTests,
                allFusionTests,
                allInterchangeAndFuseTests,
                allLoopInvariantCodeMotionTests,
                allLv2BlockingSearchTests,
                allMultiBlockingTests,
                allRegisterizeTempsTests,
                allSplitTempsTests,
                allSystemBlockingTests,
                allScalarizationTests]
