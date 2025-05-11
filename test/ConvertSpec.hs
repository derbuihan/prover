module ConvertSpec where

import Convert
import Test.Hspec
import Types

spec :: Spec
spec = do
  describe "convertProp" specConvertProp

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
