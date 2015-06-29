module Analysis.IndexExpression(IRectangle, iRectangle,
                                horizontalRange, verticalRange,
                                rectanglesOverlap,
                                iRange) where

import IndexExpression

data IRange
  = IRange IExpr IExpr
    deriving (Eq, Ord, Show)

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

iRectangle hRange vRange = IRectangle hRange vRange

horizontalRange (IRectangle h _) = h
verticalRange (IRectangle _ v) = v

rectanglesOverlap r1 r2 =
  constRangesOverlap (verticalRange r1) (verticalRange r2) &&
  constRangesOverlap (horizontalRange r1) (horizontalRange r2)

intsOverlap :: Int -> Int -> Int -> Int -> Bool
intsOverlap s1 e1 s2 e2 =
  not $ e1 < s2 || e2 < s1
