{-# LANGUAGE UnicodeSyntax #-}
module SMTLib (toSMTLib) where

import Qdimacs
import Data.List

import qualified Data.ByteString.Lazy               as L
import           Data.ByteString.Lazy.Builder
import           Data.ByteString.Lazy.Builder.ASCII (intDec)
import           Data.Monoid
import           Data.Foldable                        (foldMap)
import           Data.List                            (intersperse)

convertLiteral ∷ Literal → Builder
convertLiteral l
    | signum l == -1 = stringUtf8 "(not x" <> intDec (abs l) <> charUtf8 ')'
    | otherwise = charUtf8 'x' <> intDec l

convertClause ∷ [Literal] → Builder
convertClause [] = mempty
convertClause (c:cs) = 
    stringUtf8 "  (or "
    <> convertLiteral c <> mconcat [charUtf8 ' ' <> convertLiteral c'|c'<-cs]
    <> stringUtf8 ")\n"

convertTypedVar i = stringUtf8 "(x" <> intDec i <> stringUtf8 " Bool)"

convertQuant [] = mempty
convertQuant (c:cs) =
    convertTypedVar c <> mconcat [charUtf8 ' ' <> convertTypedVar c'|c'<-cs]

addQuant quand varBuilder formularBuilder =
    stringUtf8 " (" <> stringUtf8 quand <> stringUtf8 " (" <> varBuilder <>
    stringUtf8 ")\n" <> formularBuilder <> charUtf8 ')'

convertClauseList [] = stringUtf8 "()"
convertClauseList (c:cs) =
    stringUtf8 "(and\n"<> convertClause c <>
    mconcat [convertClause c'|c'<-cs] <> stringUtf8 ")"

convertTE ∷ [[Variable]] → [Clause] → Builder
convertTE [] c = convertClauseList c
convertTE (q:t) c 
    | q==[] = convertFA t c
    | otherwise = addQuant "exists" (convertQuant q) (convertFA t c)

convertFA ∷ [[Variable]] → [Clause] → Builder
convertFA [] c = convertClauseList c
convertFA (q:t) c 
    | q==[] = convertTE t c
    | otherwise = addQuant "forall" (convertQuant q) (convertTE t c)

toSMTLib ∷ Bool → QBFProblem → Builder
toSMTLib False (v,c) = stringUtf8 "(set-logic UF)\n(assert"
    <> (convertTE v c)
    <> stringUtf8 ")\n(check-sat)\n(exit)\n"
toSMTLib True (v,c) = stringUtf8 "(set-logic UF)\n(assert (not"
    <> (convertTE v c)
    <> stringUtf8 "))\n(check-sat)\n(exit)\n"

