module SystemTests.AllSystemTests(allSystemTests) where
import Test.HUnit

import SystemTests.BlockingTests
import SystemTests.CompactTemps
import SystemTests.Fusion
import SystemTests.InterchangeAndFuse
import SystemTests.LoopInvariantCodeMotion
import SystemTests.MultiBlocking
import SystemTests.RegisterizeTemps
import SystemTests.Scalarization

allSystemTests = TestLabel "All sytem tests" $ TestList
               [allCompactTempsTests,
                allFusionTests,
                allInterchangeAndFuseTests,
                allLoopInvariantCodeMotionTests,
                allMultiBlockingTests,
                allRegisterizeTempsTests,
                allSystemBlockingTests,
                allScalarizationTests]
