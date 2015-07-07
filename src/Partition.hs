module Partition(Partition,
                 partition,
                 Shape(..)) where

import IndexExpression

data Shape
  = Row
  | Col
    deriving (Eq, Ord, Show)

data Partition
  = Partition Shape IExpr IExpr
    deriving (Eq, Ord, Show)

partition s i l = Partition s i l


