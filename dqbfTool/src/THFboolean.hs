-- Fragment of THF to represent dependend quantified boolean formulars
-- Supports variables of type boolean, functions over booleans, the
-- connectors and, or, and negation.
-- Only variables can be applied to functions.

module THFboolean where

-- of type boolean
data Var =  Var String
  deriving (Show, Eq)

-- on booleans
data Function = Function String [Var]
