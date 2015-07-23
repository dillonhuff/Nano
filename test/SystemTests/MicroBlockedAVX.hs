module SystemTests.MicroBlockedAVX(allMicroBlockedAVXTests) where

import Test.HUnit

import CBackEnd.CodeGeneration.AVX.Common
import TestUtils

allMicroBlockedAVXTests =
  TestLabel "allMicroBlockedAVXTests" $
  TestList microBlockedAVXCases

microBlockedAVXCases =
  [ltc "matrix add unmatched strides" stmtsToAVX maddUnmatchedOpts maddUnmatched]

maddUnmatchedOpts = []

maddUnmatched = []
