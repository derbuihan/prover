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
    "fix" -> TFix
    "forallI" -> TForallIntro
    "forallE" -> TForallElim
    "existsI" -> TExistsIntro
    "existsE" -> TExistsElim
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
parseProp_ = parseForall

parseForall :: Parser Prop
parseForall (TForall : tokens) =
  let (vars, rest) = collectVarsUntilDot tokens
      (prop, rest_) = parseForall rest
   in (foldr Forall prop vars, rest_)
parseForall tokens = parseExists tokens

parseExists :: Parser Prop
parseExists (TExists : tokens) =
  let (vars, rest) = collectVarsUntilDot tokens
      (prop, rest_) = parseExists rest
   in (foldr Exists prop vars, rest_)
parseExists tokens = parseIff tokens

collectVarsUntilDot :: Parser [String]
collectVarsUntilDot (TStr s : TDot : tokens) = ([s], tokens)
collectVarsUntilDot (TStr s : tokens) =
  let (vars, rest) = collectVarsUntilDot tokens
   in (s : vars, rest)
collectVarsUntilDot tokens = error $ "Expected variable followed by dot" ++ show tokens

parseIff :: Parser Prop
parseIff tokens =
  let (left, rest) = parseImply tokens
   in case rest of
        (TIff : rest_) ->
          let (right, rest__) = parseIff rest_
           in (Iff left right, rest__)
        _ -> (left, rest)

parseImply :: Parser Prop
parseImply tokens =
  let (left, rest) = parseOr tokens
   in case rest of
        (TImp : rest_) ->
          let (right, rest__) = parseImply rest_
           in (Imp left right, rest__)
        _ -> (left, rest)

parseOr :: Parser Prop
parseOr tokens =
  let (left, rest) = parseAnd tokens
   in case rest of
        (TOr : rest_) ->
          let (right, rest__) = parseOr rest_
           in (Or left right, rest__)
        _ -> (left, rest)

parseAnd :: Parser Prop
parseAnd tokens =
  let (left, rest) = parseNot tokens
   in case rest of
        (TAnd : rest_) ->
          let (right, rest__) = parseAnd rest_
           in (And left right, rest__)
        _ -> (left, rest)

parseNot :: Parser Prop
parseNot (TNot : tokens) =
  let (prop, rest) = parseNot tokens
   in (Not prop, rest)
parseNot tokens = parseParen tokens

parseParen :: Parser Prop
parseParen (TLParen : tokens) =
  let (prop, rest) = parseProp_ tokens
   in case rest of
        (TRParen : rest_) -> (prop, rest_)
        _ -> error "Expected closing parenthesis"
parseParen tokens = parseFalsum tokens

parseFalsum :: Parser Prop
parseFalsum (TFalsum : tokens) = (Falsum, tokens)
parseFalsum tokens = parseAtom tokens

parseAtom :: Parser Prop
parseAtom (TStr s : TLParen : tokens) =
  let (terms, rest) = parseArgs tokens
   in (Atom s terms, rest)
parseAtom (TStr s : tokens) = (Atom s [], tokens)
parseAtom tokens = parseProp_ tokens

-- Parser for Terms

parseTerm :: String -> Term
parseTerm s =
  let tokens = tokenize s
      (parsed, rest) = parseTerm_ tokens
   in case rest of
        [TEOF] -> parsed
        _ -> error "Parsing failed, unexpected tokens remaining"

parseTerm_ :: Parser Term
parseTerm_ (TStr s : TLParen : tokens) =
  let (terms, rest) = parseArgs tokens
   in (Func s terms, rest)
parseTerm_ (TStr s : tokens) = (Var s, tokens)
parseTerm_ _ = error "Expected term"

parseArgs :: Parser [Term]
parseArgs [] = ([], [])
parseArgs tokens =
  let (term, rest) = parseTerm_ tokens
   in case rest of
        (TComma : rest_) ->
          let (terms, rest__) = parseArgs rest_
           in (term : terms, rest__)
        (TRParen : rest_) -> ([term], rest_)
        _ -> error "Expected comma or closing parenthesis"

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
          let (ps, rest__) = parseAssumptions_ rest_
           in (p : ps, rest__)
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
        (TFor : rest_) ->
          let (q, rest__) = parseProp_ rest_
           in (Assume p q, rest__)
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
        (TFor : rest_) ->
          let (q, rest__) = parseProp_ rest_
           in (OrElim p q, rest__)
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
parseTactic_ (TFix : tokens) =
  let (t, rest) = parseTerm_ tokens
   in (Fix t, rest)
parseTactic_ (TForallIntro : tokens) =
  let (t, rest) = parseTerm_ tokens
      (p, rest_) = parseProp_ rest
   in (ForallIntro t p, rest_)
parseTactic_ (TForallElim : tokens) =
  let (t, rest) = parseTerm_ tokens
      (p, rest_) = parseProp_ rest
   in (ForallElim t p, rest_)
parseTactic_ (TExistsIntro : tokens) =
  let (t, rest) = parseTerm_ tokens
      (p, rest_) = parseProp_ rest
   in (ExistsIntro t p, rest_)
parseTactic_ (TExistsElim : tokens) =
  let (t, rest) = parseTerm_ tokens
      (p, rest_) = parseProp_ rest
   in (ExistsElim t p, rest_)
parseTactic_ (x : _) = error $ "Invalid tactic: " ++ show x
parseTactic_ [] = error "Empty tactic"
