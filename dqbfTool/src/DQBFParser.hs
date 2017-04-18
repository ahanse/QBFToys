module DQBFParser(dqbfParser) where

import DQBF
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Token
import Text.Parsec.Language

iffS = ["<-->", "<->", "="]
ifS = ["-->", "->"]
orS = [ "\\/", "|", "+"]
andS = ["/\\", "&", "*"]
notS = ["~", "!", "~"]
xorS = ["^"]

booleanFormularDef = emptyDef{ commentStart = "",
  commentEnd = "",
  identStart = letter,
  identLetter = alphaNum,
  opStart = oneOf "<-=\\|+/&*~!-",
  opLetter = oneOf "->/\\",
  reservedOpNames = concat [iffS,ifS,orS,andS,notS, xorS],
  reservedNames = [] }

TokenParser{ parens = m_parens,
  braces = m_braces,
  identifier = m_identifier,
  reservedOp = m_reservedOp,
  whiteSpace = m_whiteSpace } = makeTokenParser booleanFormularDef

varList :: Parser [String]
varList = many m_identifier

allQuantifier :: Parser AQuant
allQuantifier = do
  char 'A'
  m_whiteSpace
  vars <- varList
  char ':'
  m_whiteSpace
  return (AQuant vars)

existsQuantifier :: Parser EQuant
existsQuantifier = do
  char 'E'
  m_whiteSpace
  deps <- m_braces varList
  vars <- many1 m_identifier
  char ':'
  m_whiteSpace
  return (EQuant deps vars)

aliasedOp l = choice $ fmap m_reservedOp l

expressionTable = [
  [Prefix (aliasedOp notS >> return Not)],
  [Infix (aliasedOp andS >> return And) AssocLeft],
  [Infix (aliasedOp orS >> return Or) AssocLeft],
  [Infix (aliasedOp ifS >> return If) AssocLeft],
  [Infix (aliasedOp xorS >> return Xor) AssocLeft],
  [Infix (aliasedOp iffS >> return Iff) AssocLeft]]
term = m_parens expression
       <|> fmap Var m_identifier

expression = buildExpressionParser expressionTable term

dqbfParser = do
  m_whiteSpace
  allQ <- many1 allQuantifier
  m_whiteSpace
  existsQ <- many1 existsQuantifier
  m_whiteSpace
  expr <- expression
  eof
  return (allQ, existsQ, expr)
