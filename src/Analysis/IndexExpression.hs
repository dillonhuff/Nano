module Analysis.IndexExpression(IRectangle, iRectangle,
                                rowRange, colRange,
                                rectanglesOverlap,
                                IRange,
                                iRange,
                                irStart, irEnd) where

import Core.IndexExpression

data IRange
  = IRange IExpr IExpr
    deriving (Eq, Ord)

instance Show IRange where
  show r = "[" ++ show (irStart r) ++ ", " ++ show (irEnd r) ++ "]"

iRange = IRange

isConstIRange (IRange s e) =
  isConst s && isConst e

irStart (IRange s _) = s
irEnd (IRange _ e) = e

constRangesOverlap r1 r2 =
  case isConstIRange r1 && isConstIRange r2 of
    True -> intsOverlap (constVal $ irStart r1) (constVal $ irEnd r1) (constVal $ irStart r2) (constVal $ irEnd r2)
    False -> False

data IRectangle
  = IRectangle IRange IRange
    deriving (Eq, Ord, Show)

iRectangle rRange cRange = IRectangle rRange cRange

rowRange (IRectangle h _) = h
colRange (IRectangle _ v) = v

rectanglesOverlap r1 r2 =
  constRangesOverlap (rowRange r1) (rowRange r2) &&
  constRangesOverlap (colRange r1) (colRange r2)

intsOverlap :: Int -> Int -> Int -> Int -> Bool
intsOverlap s1 e1 s2 e2 =
  not $ e1 < s2 || e2 < s1
