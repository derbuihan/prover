module ProofSpec where

import Proof
import Test.Hspec
import Types

spec :: Spec
spec = do
  describe "Proof" specProve

specProve :: Spec
specProve = do
  it "assum" $ do
    let input =
          ProofState
            { goal = Imply (Var "x") (Var "y"),
              assumptions = [],
              tactics = []
            }
        actual = prove (Assum (Var "x")) input
        expected =
          ProofState
            { goal = Var "y",
              assumptions = [Var "x"],
              tactics = [Assum (Var "x")]
            }
    actual `shouldBe` expected

  it "modus ponens" $ do
    let input =
          ProofState
            { goal = Var "y",
              assumptions = [Imply (Var "x") (Var "y"), Var "x"],
              tactics = [Assum (Imply (Var "x") (Var "y")), Assum (Var "x")]
            }
        actual = prove (ModusPonens (Imply (Var "x") (Var "y")) (Var "x")) input
        expected =
          ProofState
            { goal = Var "y",
              assumptions = [Var "y", Imply (Var "x") (Var "y"), Var "x"],
              tactics = [ModusPonens (Imply (Var "x") (Var "y")) (Var "x"), Assum (Imply (Var "x") (Var "y")), Assum (Var "x")]
            }
    actual `shouldBe` expected

  it "double negation elimination" $ do
    let input =
          ProofState
            { goal = Var "x",
              assumptions = [Not (Not (Var "x"))],
              tactics = []
            }
        actual = prove (DoubleNegationElim (Not (Not (Var "x")))) input
        expected =
          ProofState
            { goal = Var "x",
              assumptions = [Var "x", Not (Not (Var "x"))],
              tactics = [DoubleNegationElim (Not (Not (Var "x")))]
            }
    actual `shouldBe` expected

  it "done" $ do
    let input =
          ProofState
            { goal = Var "x",
              assumptions = [Var "x"],
              tactics = []
            }
        actual = prove Done input
        expected =
          ProofState
            { goal = Var "x",
              assumptions = [Var "x"],
              tactics = [Done]
            }
    actual `shouldBe` expected
