module Parser where

import Data.Char
import Types

-- Tokenizer

tokenize :: String -> [Token]
tokenize [] = [TEOF]
tokenize (' ' : xs) = tokenize xs
tokenize ('!' : xs) = TNot : tokenize xs
tokenize ('&' : xs) = TAnd : tokenize xs
tokenize ('|' : xs) = TOr : tokenize xs
tokenize ('(' : xs) = TLParen : tokenize xs
tokenize (')' : xs) = TRParen : tokenize xs
tokenize ('-' : '>' : xs) = TImply : tokenize xs
tokenize ('<' : '-' : '>' : xs) = TIff : tokenize xs
tokenize (x : xs)
  | isAlpha x =
      let (keyword, rest) = span isAlphaNum (x : xs)
       in (convertKeywords keyword) : tokenize rest
  | otherwise = error $ "Unknown character: " ++ [x]

convertKeywords :: String -> Token
convertKeywords keyword =
  case keyword of
    "assum" -> TAssum
    "mpp" -> TModusPonens
    "dn" -> TDoubleNegationElim
    "apply" -> TApply
    "done" -> TDone
    _ -> TVar keyword

-- Parser for Prop

type Parser a = [Token] -> (a, [Token])

parseProp :: String -> Prop
parseProp s =
  let tokens = tokenize s
      (parsed, rest) = parseProp_ tokens
   in case rest of
        [TEOF] -> parsed
        _ -> error "Parsing failed, unexpected tokens remaining"

parseProp_ :: Parser Prop
parseProp_ tokens = parseIff tokens

parseIff :: Parser Prop
parseIff tokens =
  let (left, rest) = parseImply tokens
   in case rest of
        (TIff : rest') ->
          let (right, rest'') = parseIff rest'
           in (Iff left right, rest'')
        _ -> (left, rest)

parseImply :: Parser Prop
parseImply tokens =
  let (left, rest) = parseOr tokens
   in case rest of
        (TImply : rest') ->
          let (right, rest'') = parseImply rest'
           in (Imply left right, rest'')
        _ -> (left, rest)

parseOr :: Parser Prop
parseOr tokens =
  let (left, rest) = parseAnd tokens
   in case rest of
        (TOr : rest') ->
          let (right, rest'') = parseOr rest'
           in (Or left right, rest'')
        _ -> (left, rest)

parseAnd :: Parser Prop
parseAnd tokens =
  let (left, rest) = parseNot tokens
   in case rest of
        (TAnd : rest') ->
          let (right, rest'') = parseAnd rest'
           in (And left right, rest'')
        _ -> (left, rest)

parseNot :: Parser Prop
parseNot (TNot : tokens) =
  let (prop, rest) = parseNot tokens
   in (Not prop, rest)
parseNot tokens = parseVar tokens

parseVar :: Parser Prop
parseVar (TVar var : tokens) = (Var var, tokens)
parseVar (TLParen : tokens) =
  let (prop, rest) = parseIff tokens
   in case rest of
        (TRParen : rest') -> (prop, rest')
        _ -> error "Expected closing parenthesis"
parseVar _ = error "Expected variable"

-- Parser for Tactic
parseTactic :: String -> Tactic
parseTactic s =
  let tokens = tokenize s
      (parsed, rest) = parseTactic_ tokens
   in case rest of
        [TEOF] -> parsed
        _ -> error "Parsing failed, unexpected tokens remaining"

parseTactic_ :: Parser Tactic
parseTactic_ (TAssum : tokens) =
  let (prop, rest) = parseProp_ tokens
   in (Assum prop, rest)
parseTactic_ (TModusPonens : tokens) =
  let (p, rest) = parseProp_ tokens
      (q, rest_) = parseProp_ rest
   in (ModusPonens p q, rest_)
parseTactic_ (TDoubleNegationElim : tokens) =
  let (prop, rest) = parseProp_ tokens
   in (DoubleNegationElim prop, rest)
parseTactic_ (TDone : tokens) = (Done, tokens)
parseTactic_ _ = error "Expected tactic"
