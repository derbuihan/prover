module ParserSpec where

import Parser
import Test.Hspec
import Types

spec :: Spec
spec = do
  describe "Tokenizer" specTokenize
  describe "Parser for Prop" specParseProp
  describe "Parser for Tactic" specParseTactic

specTokenize :: Spec
specTokenize = do
  it "prop" $ do
    let input = "x1 & y11 | !z123"
        actual = tokenize input
        expected = [TVar "x1", TAnd, TVar "y11", TOr, TNot, TVar "z123", TEOF]
    actual `shouldBe` expected

  it "tactic" $ do
    let input = "assum x1"
        actual = tokenize input
        expected = [TAssum, TVar "x1", TEOF]
    actual `shouldBe` expected

specParseProp :: Spec
specParseProp = do
  it "variable" $ do
    let input = "x1"
        actual = parseProp input
        expected = Var "x1"
    actual `shouldBe` expected

  it "negation" $ do
    let input = "!x1"
        actual = parseProp input
        expected = Not (Var "x1")
    actual `shouldBe` expected

  it "conjunction" $ do
    let input = "x1 & y11"
        actual = parseProp input
        expected = And (Var "x1") (Var "y11")
    actual `shouldBe` expected

  it "disjunction" $ do
    let input = "x1 | y11"
        actual = parseProp input
        expected = Or (Var "x1") (Var "y11")
    actual `shouldBe` expected

  it "complex expression" $ do
    let input = "x1 & y11 | !z123"
        actual = parseProp input
        expected = Or (And (Var "x1") (Var "y11")) (Not (Var "z123"))
    actual `shouldBe` expected

  it "implication" $ do
    let input = "x1 -> y11"
        actual = parseProp input
        expected = Imply (Var "x1") (Var "y11")
    actual `shouldBe` expected

  it "biconditional" $ do
    let input = "x1 <-> y11"
        actual = parseProp input
        expected = Iff (Var "x1") (Var "y11")
    actual `shouldBe` expected

  it "nested expressions" $ do
    let input = "x1 & (y11 | !z123)"
        actual = parseProp input
        expected = And (Var "x1") (Or (Var "y11") (Not (Var "z123")))
    actual `shouldBe` expected

  it "multiple operators" $ do
    let input = "!(x & y) <-> !x | !y"
        actual = parseProp input
        expected = Iff (Not (And (Var "x") (Var "y"))) (Or (Not (Var "x")) (Not (Var "y")))
    actual `shouldBe` expected

specParseTactic :: Spec
specParseTactic = do
  it "assume" $ do
    let input = "assum x1"
        actual = parseTactic input
        expected = Assum (Var "x1")
    actual `shouldBe` expected

  it "done" $ do
    let input = "done"
        actual = parseTactic input
        expected = Done
    actual `shouldBe` expected
