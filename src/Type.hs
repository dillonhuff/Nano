module Type(Type, single, double, isDouble, isSingle) where

data Type
  = Single
  | Double
    deriving (Eq, Ord)

instance Show Type where
  show Single = "S"
  show Double = "D"

single = Single
double = Double

isDouble Double = True
isDouble _ = False

isSingle t = not $ isDouble t
