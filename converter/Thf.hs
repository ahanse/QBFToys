module Thf (toThf) where

import Qdimacs
import Data.List

import qualified Data.ByteString.Lazy               as L
import           Data.ByteString.Lazy.Builder
import           Data.ByteString.Lazy.Builder.ASCII (intDec)
import           Data.Monoid
import           Data.Foldable                        (foldMap)
import           Data.List                            (intersperse)

convertLiteral :: Literal -> Builder
convertLiteral l
    | signum l == -1 = stringUtf8 "~(X" <> intDec (abs l) <> charUtf8 ')'
    | otherwise = charUtf8 'X' <> intDec l

convertClause :: [Literal] -> Builder
convertClause [] = mempty
convertClause (c:cs) = 
    convertLiteral c <> mconcat [charUtf8 '|' <> convertLiteral c'|c'<-cs]

convertTypedVar i = charUtf8 'X' <> intDec i <> stringUtf8 ": $o"
convertQuant [] = mempty
convertQuant (c:cs) =
    convertTypedVar c <> mconcat [charUtf8 ',' <> convertTypedVar c'|c'<-cs]

addQuant qChar varBuilder formularBuilder =
    charUtf8 '(' <> charUtf8 qChar <> stringUtf8 " [" <> varBuilder <> 
    stringUtf8 "]:" <> formularBuilder <> charUtf8 ')'

convertClauseList [] = stringUtf8 "()"
convertClauseList (c:cs) =
    stringUtf8 "(("<> convertClause c <> 
    mconcat [stringUtf8 ")&(" <> convertClause c'|c'<-cs] <> stringUtf8 "))"

convertTE :: [[Variable]] -> [Clause] -> Builder
convertTE [] c = convertClauseList c
convertTE (q:t) c 
    | q==[] = convertFA t c
    | otherwise = addQuant '?' (convertQuant q) (convertFA t c)

convertFA :: [[Variable]] -> [Clause] -> Builder
convertFA [] c = convertClauseList c
convertFA (q:t) c 
    | q==[] = convertTE t c
    | otherwise = addQuant '!' (convertQuant q) (convertFA t c)

toThf :: QBFProblem -> Builder
toThf (v,c) = stringUtf8 "thf(c,conjecture," <> (convertTE v c) <> stringUtf8 ")." 
