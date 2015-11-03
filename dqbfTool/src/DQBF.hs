module DQBF
  (AQuant(..),EQuant(..), Expression(..))
where

data AQuant = AQuant [String]
  deriving (Show,Eq)
data EQuant = EQuant [String] [String]
  deriving (Show, Eq)

data Expression = Var String |
    Not Expression |
    And Expression Expression |
    Or Expression Expression |
    Xor Expression Expression |
    If Expression Expression |
    Iff Expression Expression
  deriving (Show, Eq)
