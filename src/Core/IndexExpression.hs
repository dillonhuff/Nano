module Core.IndexExpression(IExpr,
                       evaluateIExprConstants,
                       iAdd, iMul, iConst, iVar, iSub, iDiv,
                       isConst, isVar, ieToConst,
                       allIExprOperands,
                       constVal, varName,
                       subIExpr, subIExprForVar,
                       iExprToCExpr, containsSubExpr) where

import Data.List as L

import CBackEnd.Syntax

data IExpr
  = IConst Int
  | IVar String
  | IMul IExpr IExpr
  | IAdd IExpr IExpr
  | IDiv IExpr IExpr
    deriving (Eq, Ord)

iSub l r = iAdd l (iMul (iConst (-1)) r)
iAdd l r = IAdd l r
iMul l r = IMul l r
iConst i = IConst i
iVar n = IVar n
iDiv l r = IDiv l r

isConst (IConst _) = True
isConst _ = False

isVar (IVar _) = True
isVar _ = False

constVal (IConst v) = v
varName (IVar n) = n

subIExpr target result i =
  case i == target of
    True -> result
    False -> subInSubIExprs target result i

subInSubIExprs target result (IMul l r) =
  IMul (subIExpr target result l) (subIExpr target result r)
subInSubIExprs target result (IAdd l r) =
  IAdd (subIExpr target result l) (subIExpr target result r)
subInSubIExprs _ _ i = i

instance Show IExpr where
  show (IVar n) = n
  show (IConst it) = show it
  show (IAdd l r) = "(" ++ show l ++ " + " ++ show r ++ ")"
  show (IMul l r) = "(" ++ show l ++ " * " ++ show r ++ ")"
  show (IDiv l r) = "(" ++ show l ++ " / " ++ show r ++ ")"

evaluateIExprConstants :: IExpr -> IExpr
evaluateIExprConstants (IMul l r) =
  case isConst evaledL && isConst evaledR of
    True -> iConst $ (constVal evaledL) * (constVal evaledR)
    False -> simplifyMul evaledL evaledR
  where
    evaledL = evaluateIExprConstants l
    evaledR = evaluateIExprConstants r
evaluateIExprConstants (IAdd l r) =
  case isConst evaledL && isConst evaledR of
    True -> iConst $ (constVal evaledL) + (constVal evaledR)
    False -> simplifyAdd evaledL evaledR
  where
    evaledL = evaluateIExprConstants l
    evaledR = evaluateIExprConstants r
evaluateIExprConstants (IDiv l r) =
  case isConst evaledL && isConst evaledR of
    True -> iConst $ div (constVal evaledL) (constVal evaledR)
    False -> simplifyDiv evaledL evaledR
  where
    evaledL = evaluateIExprConstants l
    evaledR = evaluateIExprConstants r    
evaluateIExprConstants e = e

simplifyDiv l r =
  case r == iConst 1 of
    True -> l
    False -> iDiv l r

simplifyMul l r =
  case l == iConst 0 || r == iConst 0 of
    True -> iConst 0
    False -> case l == iConst 1 of
      True -> r
      False -> case r == iConst 1 of
        True -> l
        False -> iMul l r

simplifyAdd l r =
  case l == iConst 0 of
    True -> r
    False -> case r == iConst 0 of
      True -> l
      False -> iAdd l r

ieToConst :: IExpr -> Maybe Int
ieToConst ie =
  let evaluatedIE = evaluateIExprConstants ie in
  case isConst evaluatedIE of
    True -> Just $ constVal evaluatedIE
    False -> Nothing

subIExprForVar :: IExpr -> String -> IExpr -> IExpr
subIExprForVar ie varName expr = subIExpr (iVar varName) ie expr

iExprToCExpr (IVar n) = cVar n
iExprToCExpr (IConst i) = cIntLit i
iExprToCExpr (IAdd l r) = cAdd (iExprToCExpr l) (iExprToCExpr r)
iExprToCExpr (IMul l r) = cMul (iExprToCExpr l) (iExprToCExpr r)
iExprToCExpr (IDiv l r) = cDiv (iExprToCExpr l) (iExprToCExpr r)

containsSubExpr subExpr i =
  case subExpr == i of
    True -> True
    False -> L.any (containsSubExpr subExpr) $ iOperands i

iOperands (IAdd l r) = [l, r]
iOperands (IMul l r) = [l, r]
iOperands (IDiv l r) = [l, r]
iOperands _ = []

allIExprOperands (IAdd l r) = allIExprOperands l ++ allIExprOperands r
allIExprOperands (IMul l r) = allIExprOperands l ++ allIExprOperands r
allIExprOperands (IDiv l r) = allIExprOperands l ++ allIExprOperands r
allIExprOperands a = [a]
