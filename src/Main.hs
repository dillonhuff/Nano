module Main(main) where

import Data.List as L

import Blocking
import CBackEnd.CodeGeneration
import CBackEnd.Syntax
import IndexExpression
import Matrix
import CBackEnd.SanityCheck
import CBackEnd.Timing
import CBackEnd.TimingHarness
import Dummies
import Search.Exhaustive
import Statement

{-main :: IO ()
main = do
  scResStr <- runSanityCheck "mainMAddTest" maddOp maddRefinedOp args
  putStrLn $ scResStr
  where
    (maddOp, args) = operationToC "matrixAdd" [plusABC]
    (maddRefinedOp, _) = operationToC "optimizedMatrixAdd" optimizedPlusABC

optimizedPlusABC = blockMatrixAddM (iVar "i") (iConst 8) plusABC

plusABC =
  matrixAdd a b c

a = constDblMat "A" 9 15 1 9
b = constDblMat "B" 9 15 15 1
c = constDblMat "C" 9 15 1 9

constDblMat name nr nc rs cs =
  matrix name (iConst nr) (iConst nc) (iConst rs) (iConst cs) (properties arg double)

-}

{-main :: IO ()
main = do
  res <- testBlocking blockScalarMultiplyM (iConst 2, smulCAlphaA)
  putStrLn $ show res

testBlocking blkFunc (blkFactor, stmt) = do
  putStrLn $ show [stmt]
  putStrLn $ show $ argInfoList [stmt]

-}

{-main :: IO ()
main =
  let res = expandStatementsBU (blockMatrixMultiplyP (iVar "k") (iConst 6)) $
            expandStatementsBU (blockMatrixMultiplyM (iVar "j") (iConst 3)) $
            expandStatementsBU (blockMatrixMultiplyN (iVar "i") (iConst 8)) $
            [matrixMultiply c b a]
      (cOp, argInfo) = operationToC "blockedMatrixMul" res in
  do
    res <- runTimingCode "mainTiming" cOp argInfo
    putStrLn res

--    putStrLn $ prettyPrint 0 $ timingHarness "blockedMatrixMul" argInfo

a = constDblMat "A" 9 9 1 9
b = constDblMat "B" 9 9 1 9
c = constDblMat "C" 9 9 1 9

alpha = constDblMat "alpha" 1 1 1 1

maddCBA = matrixAdd c b a
smulCAlphaA = scalarMultiply c alpha a

constDblMat name nr nc rs cs =
  matrix name (iConst nr) (iConst nc) (iConst rs) (iConst cs) (properties arg double)
-}

main :: IO ()
main = do
  bestOp <- search 2 someOp optimizations allCostsOne
  putStrLn $ "Cost = " ++ (show $ fst bestOp) ++ "\n" ++
             (prettyPrint 0 $ fst $ operationToC "testOp" $ snd bestOp)

someOp = [matrixAdd tr13c4 g h,
          scalarMultiply tr13c4 alpha tr13c4,
          matrixMultiply k tr13c4 i]

allCostsOne stmts =
  let (opC, argInfo) = operationToC "testOp" stmts in
  do
    timeResStr <- runTimingCode "mainTiming" opC argInfo
    return $ read $ L.head $ L.lines timeResStr

optimizations =
  L.map (\t -> expandStatementsBU t)
  [blockMatrixAddM (iVar "i1") (iConst 1),
   blockMatrixAddN (iVar "i2") (iConst 1),
   blockMatrixAddM (iVar "i3") (iConst 3),
   blockMatrixAddN (iVar "i4") (iConst 11),
   blockScalarMultiplyM (iVar "i5") (iConst 1),
   blockScalarMultiplyN (iVar "i6") (iConst 3),
   blockScalarMultiplyN (iVar "i7") (iConst 4),
   blockMatrixMultiplyM (iVar "i8") (iConst 1),
   blockMatrixMultiplyN (iVar "i9") (iConst 1),
   blockMatrixMultiplyP (iVar "i10") (iConst 1),
   blockMatrixTransposeM (iVar "i11") (iConst 1),
   blockMatrixTransposeM (iVar "i12") (iConst 6),
   blockMatrixTransposeN (iVar "i13") (iConst 3),
   blockMatrixTransposeN (iVar "i14") (iConst 1)]   
