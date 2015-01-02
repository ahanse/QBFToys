module Thf (toThf) where

import Qdimacs
import Text.Printf
import Data.List

convertLiteral :: Literal -> String
convertLiteral l
    | signum l == -1 = printf "~(X%d)" (abs l)
    | otherwise = printf "X%d" l

convertClause :: [Literal] -> String
convertClause = (intercalate "|").(map convertLiteral)

convertQuant = (intercalate ",").(map (printf "X%d: $o"))

convertTE :: [[Variable]] -> [Clause] -> String
convertTE [] c = printf "((%s))" (intercalate ")&(" (map convertClause c))
convertTE (q:t) c = printf "(? [%s]:%s)" (convertQuant q) (convertFA t c)

convertFA :: [[Variable]] -> [Clause] -> String
convertFA [] c = printf "((%s))" (intercalate ")&(" (map convertClause c))
convertFA (q:t) c = printf "(! [%s]:%s)" (convertQuant q) (convertTE t c)

toThf :: QBFProblem -> [String]
toThf (v,c) = [printf "thf(c,conjecture,%s)." (convertTE v c)]
