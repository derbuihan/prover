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
            { goal = Imp (Atom "x") (Atom "y"),
              assumptions = [],
              tactics = []
            }
        actual = prove (Assume (Atom "x")) input
        expected =
          ProofState
            { goal = Atom "y",
              assumptions = [Atom "x"],
              tactics = [Assume (Atom "x")]
            }
    actual `shouldBe` expected

  it "modus ponens" $ do
    let input =
          ProofState
            { goal = Atom "y",
              assumptions = [Imp (Atom "x") (Atom "y"), Atom "x"],
              tactics = [Assume (Imp (Atom "x") (Atom "y")), Assume (Atom "x")]
            }
        actual = prove (ImpElim (Imp (Atom "x") (Atom "y")) (Atom "x")) input
        expected =
          ProofState
            { goal = Atom "y",
              assumptions = [Atom "y", Imp (Atom "x") (Atom "y"), Atom "x"],
              tactics = [ImpElim (Imp (Atom "x") (Atom "y")) (Atom "x"), Assume (Imp (Atom "x") (Atom "y")), Assume (Atom "x")]
            }
    actual `shouldBe` expected

  it "double negation elimination" $ do
    let input =
          ProofState
            { goal = Atom "x",
              assumptions = [Not (Not (Atom "x"))],
              tactics = []
            }
        actual = prove (DnElim (Not (Not (Atom "x")))) input
        expected =
          ProofState
            { goal = Atom "x",
              assumptions = [Atom "x", Not (Not (Atom "x"))],
              tactics = [DnElim (Not (Not (Atom "x")))]
            }
    actual `shouldBe` expected

  it "done" $ do
    let input =
          ProofState
            { goal = Atom "x",
              assumptions = [Atom "x"],
              tactics = []
            }
        actual = prove Done input
        expected =
          ProofState
            { goal = Atom "x",
              assumptions = [Atom "x"],
              tactics = [Done]
            }
    actual `shouldBe` expected
