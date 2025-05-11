module ConvertSpec where

import Convert
import Test.Hspec
import Types

spec :: Spec
spec = do
  describe "convertProp" specConvertProp
  describe "parseAndConvert" specParseAndConvert

specConvertProp :: Spec
specConvertProp = do
  it "forall" $ do
    let input = Forall "x" (Atom "p" [Var "x"])
        actual = convertProp input
        expected = Forall "x" (Atom "p" [VarInt 0])
    actual `shouldBe` expected

  it "exists" $ do
    let input = Exists "x" (Atom "p" [Var "x"])
        actual = convertProp input
        expected = Exists "x" (Atom "p" [VarInt 0])
    actual `shouldBe` expected

  it "complex" $ do
    let input = Exists "x" (And (Forall "y" (Atom "p" [Var "x", Var "y"])) (Forall "z" (Atom "q" [Var "x", Var "z"])))
        actual = convertProp input
        expected = Exists "x" (And (Forall "y" (Atom "p" [VarInt 1, VarInt 0])) (Forall "z" (Atom "q" [VarInt 1, VarInt 0])))
    actual `shouldBe` expected

  it "complex2" $ do
    let input = Forall "x" (Forall "y" (Atom "p" [Var "x", VarInt 0]))
        actual = convertProp input
        expected = Forall "x" (Forall "y" (Atom "p" [VarInt 1, VarInt 0]))
    actual `shouldBe` expected

specParseAndConvert :: Spec
specParseAndConvert = do
  it "prop" $ do
    let input = "exists x. (forall y. p(x, y)) & (forall z. q(x, z))"
        actual = parseAndConvertProp input
        expected = Exists "x" (And (Forall "y" (Atom "p" [VarInt 1, VarInt 0])) (Forall "z" (Atom "q" [VarInt 1, VarInt 0])))
    actual `shouldBe` expected

  it "tactic" $ do
    let input = "assume forall x. p(x) for exists y. q(y)"
        actual = parseAndConvertTactic input
        expected = Assume (Forall "x" (Atom "p" [VarInt 0])) (Exists "y" (Atom "q" [VarInt 0]))
    actual `shouldBe` expected
