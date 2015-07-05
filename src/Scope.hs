module Scope(Scope, arg, local) where


data Scope
  = Arg
  | Local
    deriving (Eq, Ord)

instance Show Scope where
  show Arg = "A"
  show Local = "L"

arg = Arg
local = Local
