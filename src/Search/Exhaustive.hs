module Search.Exhaustive(search) where

import Data.List as L

import Statement

search :: (Monad m) =>
          Int ->
          [Statement] ->
          [[Statement] -> [Statement]] ->
          ([Statement] -> m Double) ->
          m (Double, [Statement])
search 0 operation optimizations costModel = do
  cost <- costModel operation
  return (cost, operation)
search depth operation optimizations costModel =
  let children = L.map (\optimization -> optimization operation) optimizations in
  do
    childCostsAndOperations <- sequence $ L.map (\newOperation -> search (depth - 1) newOperation optimizations costModel) children
    return $ L.head $ L.sortBy (\(c1, _) (c2, _) -> compare c1 c2) childCostsAndOperations
