module Scope(Scope, arg, local) where


data Scope
  = Arg
  | Local
    deriving (Eq, Ord, Show)

arg = Arg
local = Local
