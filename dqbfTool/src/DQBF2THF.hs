module DQBF2THF(transform) where

import qualified DQBF as D
import qualified THFdqbf as T

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Char
import Data.Maybe
import Control.Applicative((<|>), liftA)

rename (c:cs)
  | isUpper c = 'X':c:cs
  | otherwise = (toUpper c):cs

type SkolemFun = (String, T.Expression)

-- working with skolem functions
skolem ∷ String → [String] → SkolemFun
skolem name vars = (name, T.App (T.Function name' arity ) (map T.Var vars'))
  where
    name' = rename name
    vars' = map rename vars
    arity = length vars
skolemDef ∷ SkolemFun → T.Function
skolemDef (_, (T.App f _)) = f
skolemUse ∷ SkolemFun → T.Expression
skolemUse (_, f) = f
skolemDQBFName ∷ SkolemFun → String
skolemDQBFName (n, _) = n

skolemFunctions ∷ [D.EQuant] → (T.Existential, (D.Expression→Maybe T.Expression))
skolemFunctions eq = (existential, replace)
  where
    h_allE [] = []
    h_allE ((D.EQuant v []):es) = h_allE es
    h_allE ((D.EQuant v (f:fs)):es) = (skolem f v):h_allE ((D.EQuant v fs):es)
    allE = h_allE eq

    existential = T.Existential $ map skolemDef allE
    expMap = Map.fromList $ map (\f → (skolemDQBFName f, skolemUse f)) allE

    replace (D.Var s) = Map.lookup s expMap
    replace _ = Nothing

universalVars ∷ [D.AQuant] → (T.Universal, (D.Expression→Maybe T.Expression))
universalVars q = (T.Universal all , replace)
  where
    h_all [] = []
    h_all ((D.AQuant []):qs) = h_all qs
    h_all ((D.AQuant (v:vs)):qs) = (v, T.Var $ rename v):(h_all ((D.AQuant vs):qs))

    (old, all) = unzip $ h_all q
    m = Map.fromList $ zip old all

    replace (D.Var s) = liftA T.Variable $ Map.lookup s m
    replace _ = Nothing

transform ∷ Bool → ([D.AQuant], [D.EQuant], D.Expression) → T.Problem
transform invert (a,e,m) = if invert then T.Negative tu te $ transform m else T.Positive tu te $ transform m
  where
    (tu, r1)  = universalVars a
    (te, r2)  = skolemFunctions e

    transform (D.Not e) = T.Not (transform e)
    transform (D.And e1 e2) = T.And (transform e1) (transform e2)
    transform (D.Or e1 e2) = T.Or (transform e1) (transform e2)
    transform (D.Xor e1 e2) = T.Xor (transform e1) (transform e2)
    transform (D.If e1 e2) = T.If (transform e1) (transform e2)
    transform (D.Iff e1 e2) = T.Iff (transform e1) (transform e2)
    transform var = fromJust $ (r1 var) <|> (r2 var)
