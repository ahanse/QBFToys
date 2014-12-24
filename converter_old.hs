import Data.Set (Set)
import Data.Maybe
import Control.Monad
import Data.Word
import qualified Data.Set as Set
import System.IO
import Debug.Trace

type Variable = Word
-- Variable and their index in the quantifier
-- list ([∃, ∀, ∃, ∀,... , ∃])
data QuantifiedVar = QuantifiedVar Variable Int deriving Show

instance Eq QuantifiedVar where 
    (==) (QuantifiedVar a _) (QuantifiedVar b _) = a == b
instance Ord QuantifiedVar where
    compare (QuantifiedVar a _) (QuantifiedVar b _) = compare a b

type Literal = Int
type Clause = [Literal]
type QBFProblem = (Set QuantifiedVar, [Clause])

(∃) = 1
(∀) = 0

readUpto0 t = trace t $ ((map read).(takeWhile ((/=) zero)).words) t
    where zero = "0"

parseQuantifier :: Int -> String -> Maybe (Int, Set QuantifiedVar)
parseQuantifier l (h:t)
    | h=='e' && q == (∃) = Just (l,   varSet l t)
    | h=='e' && q == (∀) = Just (l+1, varSet (l+1) t)
    | h=='a' && q == (∃) = Just (l+1, varSet l t)
    | h=='a' && q == (∀) = Just (l,   varSet (l+1) t)
    where 
        q = mod l 2 
        varSet l t =  Set.fromList $ map (\x->QuantifiedVar x l) $ readUpto0 t
parseQuantifier _ _ = Nothing

parseClause "" = Nothing
parseClause t  = Just $ readUpto0 t

parseLine (l,s,c) ('c':_) = (l,s,c) 
parseLine (l,s,c) ('p':_) = (l,s,c)
parseLine (l,s,c) line@(q:_) 
    | q=='e' || q=='a' = case res of 
        Just (newLevel, s') -> (newLevel, Set.union s s', c)
        Nothing -> (l,s,c)
        where res = parseQuantifier l line
parseLine (l,s,c) t = case res of 
            Just clauses -> (l,Set.union s (setOfVars clauses), (clauses:c))
            Nothing -> (l,s,c)
        where res = parseClause t
              setOfVars c = Set.empty -- Set.fromList $ map (\x->QuantifiedVar x 1) c

main :: IO ()
main = do
    input <- getContents
    let (_,vars, clauses) = (foldl parseLine (1,Set.empty, []) $ lines input)::(Int, Set QuantifiedVar, [Clause])
    print vars
    print $ reverse clauses
