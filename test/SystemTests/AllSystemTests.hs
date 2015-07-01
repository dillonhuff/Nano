module SystemTests.AllSystemTests(allSystemTests) where

import Test.HUnit

import SystemTests.BlockingTests
import SystemTests.CompactTempsTests
import SystemTests.Fusion
import SystemTests.InterchangeAndFuse
import SystemTests.MultiBlocking
import SystemTests.Scalarization

allSystemTests = TestLabel "All sytem tests" $ TestList
               [allCompactTempsTests,
                allFusionTests,
                allInterchangeAndFuseTests,
                allMultiBlockingTests,
                allSystemBlockingTests,
                allScalarizationTests]
