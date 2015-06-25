module AllTests(allTests) where

import Test.HUnit

import AllModuleTests
import SystemTests.AllSystemTests

allTests = TestLabel "All tests" $ TestList
         [allModuleTests,
          allSystemTests]
