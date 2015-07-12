module Core.Partition(Partition,
                 partition,
                 partShape, partBase,
                 Shape(..)) where

import Core.IndexExpression

data Shape
  = Row
  | Col
    deriving (Eq, Ord, Show)

data Partition
  = Partition Shape IExpr IExpr
    deriving (Eq, Ord, Show)

partition s i l = Partition s i l

partShape (Partition s _ _) = s
partBase (Partition _ b _) = b
