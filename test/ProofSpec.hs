module ProofSpec where

import Proof
import Test.Hspec
import Types

spec :: Spec
spec = do
  describe "Proof" specProve

specProve :: Spec
specProve = do
  it "assume" $ do
    let input =
          ProofState
            { goal = Imp (Atom "x") (Atom "y"),
              assumptions = [],
              tactics = []
            }
        actual = prove (Assume (Atom "x")) input
        expected =
          ProofState
            { goal = Imp (Atom "x") (Atom "y"),
              assumptions = [Atom "x"],
              tactics = [Assume (Atom "x")]
            }
    actual `shouldBe` expected

  it "and introduction" $ do
    let input =
          ProofState
            { goal = Atom "x",
              assumptions = [Atom "x", Atom "y"],
              tactics = []
            }
        actual = prove (AndIntro (And (Atom "x") (Atom "y"))) input
        expected =
          ProofState
            { goal = Atom "x",
              assumptions = [And (Atom "x") (Atom "y"), Atom "x", Atom "y"],
              tactics = [AndIntro (And (Atom "x") (Atom "y"))]
            }
    actual `shouldBe` expected

  it "and elimination left" $ do
    let input =
          ProofState
            { goal = Atom "x",
              assumptions = [And (Atom "x") (Atom "y")],
              tactics = []
            }
        actual = prove (AndElimLeft (And (Atom "x") (Atom "y"))) input
        expected =
          ProofState
            { goal = Atom "x",
              assumptions = [Atom "x", And (Atom "x") (Atom "y")],
              tactics = [AndElimLeft (And (Atom "x") (Atom "y"))]
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
