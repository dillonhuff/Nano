module Analysis.IndexExpressionTests(allIndexExpressionTests) where

import Data.List as L
import Test.HUnit

import Analysis.IndexExpression
import Dummies
import IndexExpression
import Module

allIndexExpressionTests =
  TestLabel "All index expression analysis tests" $ TestList
  [makeTestCases regionsOverlapTest regionsOverlapCases,
   makeTestCases regionsOverlapTest regionsDontOverlapCases]

regionsOverlapTest (l, r) =
  rectanglesOverlap l r

regionsOverlapCases =
  L.map (\(x, y) -> ((x, y), True))
  [(constRect 0 3 0 3, constRect 0 3 0 3),
   (constRect 5 8 0 3, constRect 7 12 0 3),
   (constRect 10 100 5 30, constRect 98 103 20 50),
   (constRect 98 103 20 50, constRect 10 100 5 30),
   (constRect 0 3 0 3, constRect 3 6 3 6)]
  
regionsDontOverlapCases =
  L.map (\(x, y) -> ((x, y), False))
  [(constRect 0 3 0 3, constRect 5 7 5 7),
   (constRect 5 7 5 7, constRect 0 3 0 3),
   (constRect 3 4 12 17, constRect 1 2 10 11),
   (constRect 1 2 10 11, constRect 3 4 12 17)]

