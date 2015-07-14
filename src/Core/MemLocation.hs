module Core.MemLocation(MemLocation,
                        memory, register) where

data MemLocation
  = Memory
  | Register
    deriving (Eq, Ord)

instance Show MemLocation where
  show Memory = "M"
  show Register = "R"

memory = Memory
register = Register
