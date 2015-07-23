module Matching(firstToMatch) where

firstToMatch :: (Show a) => [(a -> Bool, a -> b)] -> a -> b
firstToMatch [] stmt = error $ "firstToMatch: no matches for " ++ show stmt
firstToMatch ((cond, f):rest) stmt =
  if cond stmt then f stmt else firstToMatch rest stmt
