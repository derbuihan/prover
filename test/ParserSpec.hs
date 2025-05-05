module ParserSpec where

import Parser
import Test.Hspec
import Types

spec :: Spec
spec = do
  describe "tokenizer" specTokenize
  describe "parser for Prop" specParseProp
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

specParseProp :: Spec
specParseProp = do
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
