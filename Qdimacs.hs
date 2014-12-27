module Qdimacs (Variable, 
                Literal, 
                Clause, 
                QBFProblem, 
                readProblem, 
                normalizeVars, 
                quantifieUndeclaredVars,
                isTrivial,
                toQdimacs) where 

import Data.Word
import Data.List
import Data.Maybe (fromJust)

import Data.Set (Set)
import qualified Data.Set as Set

import Text.Printf

type Variable = Word
type Literal = Int
type Clause = [Literal]
type QBFProblem = ([[Variable]], [Clause])
data Quantifier = FA | TE deriving Show

readUpto0 :: Read a => String -> [a]
readUpto0 = (map read).(takeWhile ((/=) zero)).words
    where zero = "0"
    
parseClauseLine :: String -> Maybe Clause
parseClauseLine "" = Nothing
parseClauseLine t = Just $ readUpto0 t

parseLine :: (Quantifier,[[Variable]],[Clause]) -> String ->
             (Quantifier,[[Variable]],[Clause]) 
parseLine (q,v,c) ('c':_) = (q,v,c)
parseLine (q,v,c) ('p':_) = (q,v,c)
parseLine (FA,v@(h:vt),c) (q':t)
    | q'=='e' = (TE,(readUpto0 t):v,c)
    | q'=='a' = (FA,(h++(readUpto0 t)):vt,c)
parseLine (TE,v@(h:vt),c) (q':t)
    | q'=='a' = (FA,(readUpto0 t):v,c)
    | q'=='e' = (TE,(h++(readUpto0 t)):vt,c)
parseLine (q,v,c) t = case res of 
        Just clauses -> (q,v,clauses:c)
        Nothing -> (q,v,c)
    where res = parseClauseLine t

readProblem :: [String] -> QBFProblem
readProblem l = (reverse v,reverse c)
    where (_,v,c) = foldl parseLine (TE,[[]],[]) l

data IndexedVar = IndexedVar {
                    variable :: Variable,
                    index :: Variable
                    }
instance Eq IndexedVar where
    (==) (IndexedVar a _) (IndexedVar b _) = a == b
instance Ord IndexedVar where
    compare (IndexedVar a _) (IndexedVar b _) = compare a b

onAbs :: (Word -> Word) -> Int -> Int
onAbs f i = let s = signum i in 
     ((fromIntegral.f.fromIntegral.abs) i)*s

normalizeVars :: QBFProblem -> QBFProblem
normalizeVars (vars, clauses) = (v',c')
    where ins :: Word -> Set IndexedVar -> [Variable] -> Set IndexedVar
          ins i s [] = s
          ins i s (h:t)
            | Set.member (IndexedVar h 1) s = ins i s t 
            | otherwise = ins (i+1) (Set.insert (IndexedVar h i) s) t 
          vars' = mergeEmptyQuantifier vars
          mapping = ins 1 Set.empty (concat vars')
          replace :: Variable -> Variable
          replace v = (index.fromJust) (Set.lookupLE (IndexedVar v 1) mapping)
          v' = map (map replace) vars'
          c' = map (map (onAbs replace)) clauses

quantifieUndeclaredVars :: QBFProblem -> QBFProblem
quantifieUndeclaredVars (vars@(vh:vt),c) = ((vh++newVars):vt,c)
    where varSet = Set.fromList $ concat vars 
          varsInClauses = Set.fromList $ ((map fromIntegral).concat) c
          newVars = Set.toList $ Set.difference varsInClauses varSet

type IsTrivial = Maybe Bool
trivallyTrue = (Just True, ([[1]],[[1,-1]]))
trivallyFalse = (Just False, ([[1]],[[1],[-1]]))

isTrivial :: QBFProblem -> (IsTrivial, QBFProblem)
isTrivial (_,[]) = trivallyTrue
isTrivial p@(v,c) 
    | elem [] c = trivallyFalse
    | otherwise = (Nothing, p)

toQdimacs :: QBFProblem -> [String]
toQdimacs (v,c) = (head:vars)++clauses
    where head = printf "p cnf %d %d" numVars (length c)
          vars = [q++l | (q,l)<-(zip quant (map toLine v))]
          clauses = map toLine c
          numVars = Set.size $ Set.fromList (concat v)
          toLine l = (intercalate " " $ map show l)++" 0"
          quant = [if (odd) i then "e " else "a " | i<-[1..]]

mergeEmptyQuantifier :: [[Variable]] -> [[Variable]]
mergeEmptyQuantifier (a:[]:c:t) = (a++c):(mergeEmptyQuantifier t)
mergeEmptyQuantifier [a,[]] = [a]
mergeEmptyQuantifier [a] = [a]
mergeEmptyQuantifier [] = []
mergeEmptyQuantifier (h:t) = h:(mergeEmptyQuantifier t)
