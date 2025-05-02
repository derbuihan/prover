module ProofSpec where

import Proof
import Test.Hspec
import Types

spec :: Spec
spec = do
  describe "Proof" specProve
  describe "Helpers" specHelpers

specProve :: Spec
specProve = do
  it "assume" $ do
    let input =
          ProofState
            { goal = Imp (Atom "x") (Atom "y"),
              assumptions = [],
              subProofs = [],
              tactics = []
            }
        actual = prove (Assume (Atom "x")) input
        expected =
          ProofState
            { goal = Imp (Atom "x") (Atom "y"),
              assumptions = [Atom "x"],
              subProofs = [],
              tactics = [Assume (Atom "x")]
            }
    actual `shouldBe` expected

  it "and introduction" $ do
    let input =
          ProofState
            { goal = Atom "x",
              assumptions = [Atom "x", Atom "y"],
              subProofs = [],
              tactics = []
            }
        actual = prove (AndIntro (And (Atom "x") (Atom "y"))) input
        expected =
          ProofState
            { goal = Atom "x",
              assumptions = [And (Atom "x") (Atom "y"), Atom "x", Atom "y"],
              subProofs = [],
              tactics = [AndIntro (And (Atom "x") (Atom "y"))]
            }
    actual `shouldBe` expected

  it "and elimination left" $ do
    let input =
          ProofState
            { goal = Atom "x",
              assumptions = [And (Atom "x") (Atom "y")],
              subProofs = [],
              tactics = []
            }
        actual = prove (AndElimLeft (And (Atom "x") (Atom "y"))) input
        expected =
          ProofState
            { goal = Atom "x",
              assumptions = [Atom "x", And (Atom "x") (Atom "y")],
              subProofs = [],
              tactics = [AndElimLeft (And (Atom "x") (Atom "y"))]
            }
    actual `shouldBe` expected

  it "done" $ do
    let input =
          ProofState
            { goal = Atom "x",
              assumptions = [Atom "x"],
              subProofs = [],
              tactics = []
            }
        actual = prove Done input
        expected =
          ProofState
            { goal = Atom "x",
              assumptions = [Atom "x"],
              subProofs = [],
              tactics = [Done]
            }
    actual `shouldBe` expected

specHelpers :: Spec
specHelpers = do
  it "isInAssumptions" $ do
    let input =
          ProofState
            { goal = Atom "x",
              assumptions = [Atom "x", Atom "y"],
              subProofs = [],
              tactics = []
            }
        actual = isInAssumptions (Atom "x") input
        expected = True
    actual `shouldBe` expected

  it "isProved" $ do
    let input =
          ProofState
            { goal = Atom "x",
              assumptions = [Atom "x"],
              subProofs = [],
              tactics = []
            }
        actual = isProved input
        expected = False
    actual `shouldBe` expected

  it "isProved with Done" $ do
    let input =
          ProofState
            { goal = Atom "x",
              assumptions = [Atom "x"],
              subProofs = [],
              tactics = [Done]
            }
        actual = isProved input
        expected = True
    actual `shouldBe` expected
