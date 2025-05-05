module Parser where

import Data.Char
import GHC.Conc (par)
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
tokenize (',' : xs) = TComma : tokenize xs
tokenize ('.' : xs) = TDot : tokenize xs
tokenize ('-' : '>' : xs) = TImp : tokenize xs
tokenize ('<' : '-' : '>' : xs) = TIff : tokenize xs
tokenize (x : xs)
  | isAlpha x =
      let (keyword, rest) = span isAlphaNum (x : xs)
       in convertKeywords keyword : tokenize rest
  | otherwise = error $ "Unknown character: " ++ [x]

convertKeywords :: String -> Token
convertKeywords keyword =
  case keyword of
    "False" -> TFalsum
    "Falsum" -> TFalsum
    "⊥" -> TFalsum
    "forall" -> TForall
    "exists" -> TExists
    "assume" -> TAssume
    "for" -> TFor
    "suppose" -> TSuppose
    "andI" -> TAndIntro
    "andEL" -> TAndElimLeft
    "andER" -> TAndElimRight
    "orI" -> TOrIntro
    "orE" -> TOrElim
    "impI" -> TImpIntro
    "cp" -> TImpIntro
    "impE" -> TImpElim
    "mpp" -> TImpElim
    "dn" -> TDn
    "contra" -> TContra
    "done" -> TDone
    _ -> TStr keyword

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
parseProp_ = parseIff

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
        (TImp : rest') ->
          let (right, rest'') = parseImply rest'
           in (Imp left right, rest'')
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
parseNot tokens = parseParents tokens

parseParents :: Parser Prop
parseParents (TLParen : tokens) =
  let (prop, rest) = parseIff tokens
   in case rest of
        (TRParen : rest') -> (prop, rest')
        _ -> error "Expected closing parenthesis"
parseParents tokens = parseFalsum tokens

parseFalsum :: Parser Prop
parseFalsum (TFalsum : tokens) = (Falsum, tokens)
parseFalsum tokens = parseAtom tokens

parseAtom :: Parser Prop
parseAtom (TStr s : tokens) = (Atom s [], tokens)
parseAtom _ = error "Expected atom"

-- Parser for Assumptions

parseAssumptions :: String -> [Prop]
parseAssumptions "" = []
parseAssumptions s =
  let tokens = tokenize s
      (parsed, rest) = parseAssumptions_ tokens
   in case rest of
        [TEOF] -> parsed
        _ -> error "Parsing failed, unexpected tokens remaining"

parseAssumptions_ :: Parser [Prop]
parseAssumptions_ tokens =
  let (p, rest) = parseProp_ tokens
   in case rest of
        (TComma : rest_) ->
          let (ps, rest'') = parseAssumptions_ rest_
           in (p : ps, rest'')
        (TEOF : _) ->
          ([p], rest)
        _ ->
          ([p], rest)

-- Parser for Tactic

parseTactic :: String -> Tactic
parseTactic s =
  let tokens = tokenize s
      (parsed, rest) = parseTactic_ tokens
   in case rest of
        [TEOF] -> parsed
        _ -> error "Parsing failed, unexpected tokens remaining"

parseTactic_ :: Parser Tactic
parseTactic_ (TAssume : tokens) =
  let (p, rest) = parseProp_ tokens
   in case rest of
        (TFor : rest') ->
          let (q, rest'') = parseProp_ rest'
           in (Assume p q, rest'')
        _ -> error "Expected 'for' after 'assume'"
parseTactic_ (TSuppose : tokens) =
  let (p, rest) = parseProp_ tokens
   in (Suppose p, rest)
parseTactic_ (TAndIntro : tokens) =
  let (p, rest) = parseProp_ tokens
   in (AndIntro p, rest)
parseTactic_ (TAndElimLeft : tokens) =
  let (p, rest) = parseProp_ tokens
   in (AndElimLeft p, rest)
parseTactic_ (TAndElimRight : tokens) =
  let (p, rest) = parseProp_ tokens
   in (AndElimRight p, rest)
parseTactic_ (TOrIntro : tokens) =
  let (p, rest) = parseProp_ tokens
   in (OrIntro p, rest)
parseTactic_ (TOrElim : tokens) =
  let (p, rest) = parseProp_ tokens
   in case rest of
        (TFor : rest') ->
          let (q, rest'') = parseProp_ rest'
           in (OrElim p q, rest'')
        _ -> error "Expected 'for' after 'orE'"
parseTactic_ (TImpIntro : tokens) =
  let (p, rest) = parseProp_ tokens
   in (ImpIntro p, rest)
parseTactic_ (TImpElim : tokens) =
  let (p, rest) = parseProp_ tokens
      (q, rest_) = parseProp_ rest
   in (ImpElim p q, rest_)
parseTactic_ (TDn : tokens) =
  let (p, rest) = parseProp_ tokens
   in (Dn p, rest)
parseTactic_ (TContra : tokens) =
  let (p, rest) = parseProp_ tokens
      (q, rest_) = parseProp_ rest
   in (Contra p q, rest_)
parseTactic_ (TDone : tokens) = (Done, tokens)
parseTactic_ (x : _) = error $ "Invalid tactic: " ++ show x
parseTactic_ [] = error "Empty tactic"
