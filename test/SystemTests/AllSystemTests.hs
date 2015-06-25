module SystemTests.AllSystemTests(allSystemTests) where

import Test.HUnit

import SystemTests.BlockingTests
import SystemTests.Fusion
import SystemTests.MultiBlocking

allSystemTests = TestLabel "All sytem tests" $ TestList
               [allSystemBlockingTests,
                allMultiBlockingTests,
                allFusionTests]
