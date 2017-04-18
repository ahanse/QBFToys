-- represent dqubf problems in thf language
module THFdqbf (
  Var(..), Function(..),Expression(..),
  Universal(..),Existential(..), Problem(..),
  build, buildThf)
where

import qualified Data.ByteString.Lazy as L
import Data.ByteString.Lazy.Builder
import Data.ByteString.Lazy.Builder.ASCII (intDec)
import Data.Monoid
import Data.Foldable (foldMap)
import Data.List (intersperse)

-- class for pretty building
class Pp a where
  build ∷ a → Builder

-- in DQBF only vars are applied to funcitons
-- Identifier have to start with an uppercase letter!
data Var = Var String
  deriving (Show, Eq)
data Function = Function String Int
  deriving (Show, Eq)

instance Pp Var where
  build (Var s) = stringUtf8 s
instance Pp Function where
  build (Function n a) = stringUtf8 n <> stringUtf8 ": ("
    <> ntypes (a+1) <> rb
    where
      ntypes 1 = stringUtf8 "$o"
      ntypes n = stringUtf8 "$o > " <> ntypes (n-1)

data Expression  =
  Variable Var |
  App Function [Var] |
  Not Expression |
  And Expression Expression |
  Or Expression Expression |
  Xor Expression Expression |
  If Expression Expression |
  Iff Expression Expression
    deriving (Show, Eq)

instance Pp Expression where
  build (Variable v) = build v
  build (App (Function f _) vs) = lb <> stringUtf8 f <> mconcat [stringUtf8 " @ " <> build v' | v'←vs] <> rb
  build (Not e) = stringUtf8 "~("<> build e <> rb
  build (And e1 e2) = lb <> build  e1 <> stringUtf8 " & " <> build e2 <> rb
  build (Or e1 e2) = lb <> build  e1 <> stringUtf8 " | " <> build e2 <> rb
  build (If e1 e2) = lb <> build  e1 <> stringUtf8 " => " <> build e2 <> rb
  build (Iff e1 e2) = lb <> build  e1 <> stringUtf8 " <=> " <> build e2 <> rb
  build (Xor e1 e2) = stringUtf8 "~(" <> build  e1 <> stringUtf8 " <=> " <> build e2 <> rb

data Universal = Universal [Var]
  deriving (Show, Eq)
data Existential = Existential [Function]
  deriving (Show, Eq)

instance Pp Universal where
  build (Universal v) = stringUtf8 "![" <> vars v <> stringUtf8 "]:"
    where
      var v = build v <> stringUtf8 ": $o"
      vars [] = mempty
      vars (v:vs) = var v <> mconcat [charUtf8 ',' <> var v'|v'←vs]
instance Pp Existential where
  build (Existential f) = stringUtf8 "?[" <> ext f <> stringUtf8 "]:"
    where ext (e:es) = build e <> mconcat [charUtf8 ',' <> build e' | e'←es]

data Problem =
  Positive Universal Existential Expression |
  Negative Universal Existential Expression
  deriving (Show, Eq)

out ∷ String → Universal → Existential → Expression → Builder
out c u e m = stringUtf8 "thf(c,conjecture," <> stringUtf8 c <>
  build u <> lb <> build e <> lb <> build m <> (mconcat $ replicate 4 rb) <> charUtf8 '.'

buildThf ∷ Problem → Builder
buildThf (Positive u e m) = out "(" u e m
buildThf (Negative u e m) = out "~(" u e m

-- helper
lb = charUtf8 '('
rb = charUtf8 ')'
