module Core.MemLocation(MemLocation,
                        memory, registerGroup,
                        isRegisterGroup, isMemory,
                        RegFormat(..),
                        regGroupFormat) where

data MemLocation
  = Memory
  | RegisterGroup RegFormat
    deriving (Eq, Ord)

instance Show MemLocation where
  show Memory = "M"
  show (RegisterGroup fmt) = "R" ++ show fmt

memory = Memory
registerGroup = RegisterGroup

regGroupFormat (RegisterGroup rf) = rf

isRegisterGroup (RegisterGroup _) = True
isRegisterGroup _ = False

isMemory m = not $ isRegisterGroup m

data RegFormat
  = ByRow
  | ByCol
    deriving (Eq, Ord, Show)
