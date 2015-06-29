module Analysis.Loop(sameIterationSpace,
                     numIterationsConst,
                     isInnerLoop) where

import Data.List as L

import IndexExpression
import Statement

sameIterationSpace :: Statement -> Statement -> Bool
sameIterationSpace leftLoop rightLoop =
  (loopStart leftLoop) == (loopStart rightLoop) &&
  (loopInc leftLoop) == (loopInc rightLoop) &&
  (loopEnd leftLoop) == (loopEnd rightLoop)

isInnerLoop :: Statement -> Bool
isInnerLoop stmt =
  isLoop stmt && (noDeeperLoops stmt)

noDeeperLoops l =
  L.and $ L.map (\stmt -> not $ isLoop stmt) $ loopBody l

numIterationsConst :: Statement -> Int
numIterationsConst l =
  let s = constVal $ loopStart l
      e = constVal $ loopEnd l
      i = constVal $ loopInc l in
  (div (e - s) i)*i
