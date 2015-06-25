module Analysis.Loop(sameIterationSpace,
                     isInnerLoop) where

import Data.List as L

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
