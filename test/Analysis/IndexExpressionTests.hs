module Analysis.IndexExpressionTests(allIndexExpressionTests) where

import Data.List as L
import Test.HUnit

import Analysis.IndexExpression
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
  [(constIRectangle 0 3 0 3, constIRectangle 0 3 0 3),
   (constIRectangle 5 8 0 3, constIRectangle 7 12 0 3),
   (constIRectangle 10 100 5 30, constIRectangle 98 103 20 50),
   (constIRectangle 98 103 20 50, constIRectangle 10 100 5 30),
   (constIRectangle 0 3 0 3, constIRectangle 3 6 3 6)]
  
regionsDontOverlapCases =
  L.map (\(x, y) -> ((x, y), False))
  [(constIRectangle 0 3 0 3, constIRectangle 5 7 5 7),
   (constIRectangle 5 7 5 7, constIRectangle 0 3 0 3),
   (constIRectangle 3 4 12 17, constIRectangle 1 2 10 11),
   (constIRectangle 1 2 10 11, constIRectangle 3 4 12 17)]

constIRectangle :: Int -> Int -> Int -> Int -> IRectangle
constIRectangle r1 c1 r2 c2 =
  iRectangle (iRange (iConst r1) (iConst c1)) (iRange (iConst r2) (iConst c2))
