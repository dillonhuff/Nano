module FrontEnd.ParserTests(allParserTests) where

import Data.List as L
import Test.HUnit

import Dummies
import IndexExpression
import FrontEnd.Lexer
import FrontEnd.Parser
import FrontEnd.Token
import Matrix
import MatrixOperation
import Module

allParserTests = TestLabel "allParserTests" $ TestList 
--  [testFunction (lexAndParseOperation "noname.lspc") opCases,
--   testFunction (lexAndParseStatement "noname.lspc") stCases,
   [makeTestCases (lexAndParseFormalParam "noname.lspc") formalParamCases]

{-opCases =
  L.map (\(x, y) -> (x, Right [y]))
  [("operation nothing() { }",
    matrixOperation "nothing" [] [] dummyPos),
   ("operation trans(output matrix double 12 32 1 12 b, r matrix double 32 12 1 32 a) {}",
    matrixOperation "trans" [("b", mOpSymInfo arg doubleFloat (layout (iConst 12) (iConst 32) (iConst 1) (iConst 12))),
                             ("a", mOpSymInfo arg doubleFloat (layout (iConst 32) (iConst 12) (iConst 1) (iConst 32)))] [] dummyPos),
   ("operation madd(rw matrix float 12 32 1 12 b, r matrix float 12 32 1 32 a, output matrix float 12 32 32 1 c) { c = a + b; }",
    matrixOperation "madd" [("b", mOpSymInfo arg singleFloat (layout (iConst 12) (iConst 32) (iConst 1) (iConst 12))),
                             ("a", mOpSymInfo arg singleFloat (layout (iConst 12) (iConst 32) (iConst 1) (iConst 32))),
                             ("c", mOpSymInfo arg singleFloat (layout (iConst 12) (iConst 32) (iConst 32) (iConst 1)))]
    [dMatAsg "c" (dMatrixAdd (dMatName "a") (dMatName "b"))] dummyPos)]

stCases =
  L.map (\(x, y) -> (x, Right y))
  [("c = a + b;", dMatAsg "c" (dMatrixAdd (dMatName "a") (dMatName "b"))),
   ("c = a - b;", dMatAsg "c" (dMatrixSub (dMatName "a") (dMatName "b"))),
   ("c = a * b;", dMatAsg "c" (dMatrixMul (dMatName "a") (dMatName "b"))),
   ("y = alpha*x + y;", dMatAsg "y" (dMatrixAdd (dMatrixMul (dMatName "alpha") (dMatName "x")) (dMatName "y"))),
   ("x = b';", dMatAsg "x" (dMatrixTrans (dMatName "b"))),
   ("K = A*(B + C);", dMatAsg "K" (dMatrixMul (dMatName "A") (dMatrixAdd (dMatName "B") (dMatName "C")))),
   ("x = (beta + (alpha + omega));", dMatAsg "x" (dMatrixAdd (dMatName "beta") (dMatrixAdd (dMatName "alpha") (dMatName "omega")))),
   ("A = alpha .* A;", dMatAsg "A" (dScalarMul (dMatName "alpha") (dMatName "A")))]-}

formalParamCases =
  L.map (\(x, y) -> (x, Right y))
  [("iarg scal double c", ("c", constDblMat "c" 1 1 1 1)),
   ("temp scal double c", ("c", constDblMatTemp "c" 1 1 1 1)),
   ("oarg scal single c", ("c", constFltMat "c" 1 1 1 1)),
   ("oarg rvec 12 double xVec", ("xVec", constDblMat "xVec" 1 12 1 1)),
   ("iarg cvec 19 single yVec", ("yVec", constFltMat "yVec" 19 1 1 1)),
   ("iarg cvec n double yVec",
    ("yVec", argDblMat "yVec" (iVar "n") (iConst 1) (iConst 1) (iConst 1))),
   ("iarg matx rm 12 34 single A",
    ("A", constFltMat "A" 12 34 34 1)),
   ("iarg matx cm m n single A",
    ("A", argFltMat "A" (iVar "m") (iVar "n") (iConst 1) (iVar "m")))]

argDblMat n nr nc rs cs =
  matrix n nr nc rs cs (properties arg double memory)

argFltMat n nr nc rs cs =
  matrix n nr nc rs cs (properties arg single memory)

--lexAndParseOperation fName str = (lexString fName str) >>= (parseOperation fName)
--lexAndParseStatement fName str = (lexString fName str) >>= (parseStatement fName)
lexAndParseFormalParam fName str = (lexString fName str) >>= (parseFormalParam fName)
