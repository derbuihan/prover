module ParserSpec where

import Parser
import Test.Hspec
import Types

spec :: Spec
spec = do
  describe "tokenizer" specTokenize
  describe "parser for Propositional Logic" specParsePropLogic
  describe "parser for Predicate Logic" specParsePredLogic
  describe "parser for Term" specParseTerm
  describe "parser for Assumptions" specParseAssumptions
  describe "parser for Tactic" specParseTactic

specTokenize :: Spec
specTokenize = do
  it "prop" $ do
    let input = "x1 & y11 | !z123"
        actual = tokenize input
        expected = [TStr "x1", TAnd, TStr "y11", TOr, TNot, TStr "z123", TEOF]
    actual `shouldBe` expected

  it "tactic" $ do
    let input = "assume x1"
        actual = tokenize input
        expected = [TAssume, TStr "x1", TEOF]
    actual `shouldBe` expected

specParsePropLogic :: Spec
specParsePropLogic = do
  it "falsum" $ do
    let input = "False"
        actual = parseProp input
        expected = Falsum
    actual `shouldBe` expected

  it "atom" $ do
    let input = "x1"
        actual = parseProp input
        expected = Atom "x1" []
    actual `shouldBe` expected

  it "negation" $ do
    let input = "!x1"
        actual = parseProp input
        expected = Not (Atom "x1" [])
    actual `shouldBe` expected

  it "conjunction" $ do
    let input = "x1 & y11"
        actual = parseProp input
        expected = And (Atom "x1" []) (Atom "y11" [])
    actual `shouldBe` expected

  it "disjunction" $ do
    let input = "x1 | y11"
        actual = parseProp input
        expected = Or (Atom "x1" []) (Atom "y11" [])
    actual `shouldBe` expected

  it "complex expression" $ do
    let input = "x1 & y11 | !z123"
        actual = parseProp input
        expected = Or (And (Atom "x1" []) (Atom "y11" [])) (Not (Atom "z123" []))
    actual `shouldBe` expected

  it "implication" $ do
    let input = "x1 -> y11"
        actual = parseProp input
        expected = Imp (Atom "x1" []) (Atom "y11" [])
    actual `shouldBe` expected

  it "biconditional" $ do
    let input = "x1 <-> y11"
        actual = parseProp input
        expected = Iff (Atom "x1" []) (Atom "y11" [])
    actual `shouldBe` expected

  it "nested expressions" $ do
    let input = "x1 & (y11 | !z123)"
        actual = parseProp input
        expected = And (Atom "x1" []) (Or (Atom "y11" []) (Not (Atom "z123" [])))
    actual `shouldBe` expected

  it "multiple operators" $ do
    let input = "!(x & y) <-> !x | !y"
        actual = parseProp input
        expected = Iff (Not (And (Atom "x" []) (Atom "y" []))) (Or (Not (Atom "x" [])) (Not (Atom "y" [])))
    actual `shouldBe` expected

specParsePredLogic :: Spec
specParsePredLogic = do
  it "forall" $ do
    let input = "forall x. P(x)"
        actual = parseProp input
        expected = Forall "x" (Atom "P" [Var "x"])
    actual `shouldBe` expected

  it "forall complex" $ do
    let input = "forall x. P(x, f(y, z)) & Q(w)"
        actual = parseProp input
        expected = Forall "x" (And (Atom "P" [Var "x", Func "f" [Var "y", Var "z"]]) (Atom "Q" [Var "w"]))
    actual `shouldBe` expected

  it "exists" $ do
    let input = "exists x. Q(x)"
        actual = parseProp input
        expected = Exists "x" (Atom "Q" [Var "x"])
    actual `shouldBe` expected

  it "exists complex" $ do
    let input = "exists x. P(x) | Q(f(y, z), w)"
        actual = parseProp input
        expected = Exists "x" (Or (Atom "P" [Var "x"]) (Atom "Q" [Func "f" [Var "y", Var "z"], Var "w"]))
    actual `shouldBe` expected

  it "nested quantifiers" $ do
    let input = "forall x. exists y. forall z. P(x, y, z)"
        actual = parseProp input
        expected = Forall "x" (Exists "y" (Forall "z" (Atom "P" [Var "x", Var "y", Var "z"])))
    actual `shouldBe` expected

  it "multiple forall" $ do
    let input = "forall x y z. P(x, y, z)"
        actual = parseProp input
        expected = Forall "x" (Forall "y" (Forall "z" (Atom "P" [Var "x", Var "y", Var "z"])))
    actual `shouldBe` expected

  it "multiple exists" $ do
    let input = "exists x y z. P(x, y, z)"
        actual = parseProp input
        expected = Exists "x" (Exists "y" (Exists "z" (Atom "P" [Var "x", Var "y", Var "z"])))
    actual `shouldBe` expected

specParseTerm :: Spec
specParseTerm = do
  it "variable" $ do
    let input = "x"
        actual = parseTerm input
        expected = Var "x"
    actual `shouldBe` expected

  it "function" $ do
    let input = "f(x, y)"
        actual = parseTerm input
        expected = Func "f" [Var "x", Var "y"]
    actual `shouldBe` expected

  it "nested function" $ do
    let input = "f(g(x), h(x, y), z)"
        actual = parseTerm input
        expected = Func "f" [Func "g" [Var "x"], Func "h" [Var "x", Var "y"], Var "z"]
    actual `shouldBe` expected

specParseAssumptions :: Spec
specParseAssumptions = do
  it "single assumption" $ do
    let input = "x1"
        actual = parseAssumptions input
        expected = [Atom "x1" []]
    actual `shouldBe` expected

  it "multiple assumptions" $ do
    let input = "x1, y11, !z123"
        actual = parseAssumptions input
        expected = [Atom "x1" [], Atom "y11" [], Not (Atom "z123" [])]
    actual `shouldBe` expected

  it "empty assumptions" $ do
    let input = ""
        actual = parseAssumptions input
        expected = []
    actual `shouldBe` expected

specParseTactic :: Spec
specParseTactic = do
  it "assume" $ do
    let input = "assume x1 for x2"
        actual = parseTactic input
        expected = Assume (Atom "x1" []) (Atom "x2" [])
    actual `shouldBe` expected

  it "done" $ do
    let input = "done"
        actual = parseTactic input
        expected = Done
    actual `shouldBe` expected

  it "forall intro" $ do
    let input = "forallI x p(x)"
        actual = parseTactic input
        expected = ForallIntro (Var "x") (Atom "p" [Var "x"])
    actual `shouldBe` expected

  it "exists elim" $ do
    let input = "existsE a p(a) for q"
        actual = parseTactic input
        expected = ExistsElim (Var "a") (Atom "p" [Var "a"]) (Atom "q" [])
    actual `shouldBe` expected
