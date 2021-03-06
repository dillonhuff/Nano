module SystemTests.AllSystemTests(allSystemTests) where

import Test.HUnit

import SystemTests.AVXCodeGen
import SystemTests.BasicGS
import SystemTests.BlockingTests
import SystemTests.CompactTemps
import SystemTests.Fusion
import SystemTests.InterchangeAndFuse
import SystemTests.LoopInvariantCodeMotion
import SystemTests.FltLv2BlockingSearch
import SystemTests.Lv2BlockingSearch
import SystemTests.MicroBlockedAVX
import SystemTests.MultiBlocking
import SystemTests.RegisterizeTemps
import SystemTests.Scalarization
import SystemTests.SplitTemps

allSystemTests = TestLabel "All sytem tests" $ TestList
               [allAVXCodeGenTests,
                allBasicGSTests,
                allCompactTempsTests,
                allFusionTests,
                allInterchangeAndFuseTests,
                allLoopInvariantCodeMotionTests,
                allFltLv2BlockingSearchTests,
                allLv2BlockingSearchTests,
                allMicroBlockedAVXTests,
                allMultiBlockingTests,
                allRegisterizeTempsTests,
                allSplitTempsTests,
                allSystemBlockingTests,
                allScalarizationTests]
