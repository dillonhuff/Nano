module Analysis.Loop(sameIterationSpace) where

import Statement

sameIterationSpace :: Statement -> Statement -> Bool
sameIterationSpace leftLoop rightLoop =
  (loopStart leftLoop) == (loopStart rightLoop) &&
  (loopInc leftLoop) == (loopInc rightLoop) &&
  (loopEnd leftLoop) == (loopEnd rightLoop)
