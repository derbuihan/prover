module ProveSpec where

import Prove
import Test.Hspec
import Types

spec :: Spec
spec = do
  describe "prove" specProve
  describe "helpers" specHelpers

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
            { goal = Atom "y",
              assumptions = [Atom "x"],
              subProofs = [],
              tactics = [Assume (Atom "x")]
            }
    actual `shouldBe` expected

  it "suppose" $ do
    let input =
          ProofState
            { goal = Atom "x",
              assumptions = [],
              subProofs = [],
              tactics = []
            }
        actual = prove (Suppose (Atom "x")) input
        expected =
          ProofState
            { goal = Atom "x",
              assumptions = [Not (Atom "x")],
              subProofs =
                [ ProofState
                    { goal = Falsum,
                      assumptions = [Atom "x"],
                      subProofs = [],
                      tactics = []
                    }
                ],
              tactics = [Suppose (Atom "x")]
            }
    actual `shouldBe` expected

  it "andI" $ do
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

  it "andEL" $ do
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

  it "andER" $ do
    let input =
          ProofState
            { goal = Atom "x",
              assumptions = [And (Atom "x") (Atom "y")],
              subProofs = [],
              tactics = []
            }
        actual = prove (AndElimRight (And (Atom "x") (Atom "y"))) input
        expected =
          ProofState
            { goal = Atom "x",
              assumptions = [Atom "y", And (Atom "x") (Atom "y")],
              subProofs = [],
              tactics = [AndElimRight (And (Atom "x") (Atom "y"))]
            }
    actual `shouldBe` expected

  it "orI" $ do
    let input =
          ProofState
            { goal = Atom "x",
              assumptions = [Atom "x"],
              subProofs = [],
              tactics = []
            }
        actual = prove (OrIntro (Or (Atom "x") (Atom "y"))) input
        expected =
          ProofState
            { goal = Atom "x",
              assumptions = [Or (Atom "x") (Atom "y"), Atom "x"],
              subProofs = [],
              tactics = [OrIntro (Or (Atom "x") (Atom "y"))]
            }
    actual `shouldBe` expected

  it "orE" $ do
    let input =
          ProofState
            { goal = Atom "x",
              assumptions = [Or (Atom "x") (Atom "y"), Imp (Atom "x") (Atom "z"), Imp (Atom "y") (Atom "z")],
              subProofs = [],
              tactics = []
            }
        actual = prove (OrElim (Or (Atom "x") (Atom "y")) (Imp (Atom "x") (Atom "z")) (Imp (Atom "y") (Atom "z"))) input
        expected =
          ProofState
            { goal = Atom "x",
              assumptions = [Atom "z", Or (Atom "x") (Atom "y"), Imp (Atom "x") (Atom "z"), Imp (Atom "y") (Atom "z")],
              subProofs = [],
              tactics = [OrElim (Or (Atom "x") (Atom "y")) (Imp (Atom "x") (Atom "z")) (Imp (Atom "y") (Atom "z"))]
            }
    actual `shouldBe` expected

  it "impI (cp)" $ do
    let input =
          ProofState
            { goal = Atom "z",
              assumptions = [Atom "x", Atom "y"],
              subProofs = [],
              tactics = []
            }
        actual = prove (ImpIntro (Imp (Atom "x") (Atom "y"))) input
        expected =
          ProofState
            { goal = Atom "z",
              assumptions = [Imp (Atom "x") (Atom "y"), Atom "x", Atom "y"],
              subProofs = [],
              tactics = [ImpIntro (Imp (Atom "x") (Atom "y"))]
            }
    actual `shouldBe` expected

  it "impE (mpp)" $ do
    let input =
          ProofState
            { goal = Atom "y",
              assumptions = [Atom "x", Imp (Atom "x") (Atom "y")],
              subProofs = [],
              tactics = []
            }
        actual = prove (ImpElim (Atom "x") (Imp (Atom "x") (Atom "y"))) input
        expected =
          ProofState
            { goal = Atom "y",
              assumptions = [Atom "y", Atom "x", Imp (Atom "x") (Atom "y")],
              subProofs = [],
              tactics = [ImpElim (Atom "x") (Imp (Atom "x") (Atom "y"))]
            }
    actual `shouldBe` expected

  it "dn" $ do
    let input =
          ProofState
            { goal = Atom "x",
              assumptions = [Not (Not (Atom "x"))],
              subProofs = [],
              tactics = []
            }
        actual = prove (Dn (Not (Not (Atom "x")))) input
        expected =
          ProofState
            { goal = Atom "x",
              assumptions = [Atom "x", Not (Not (Atom "x"))],
              subProofs = [],
              tactics = [Dn (Not (Not (Atom "x")))]
            }
    actual `shouldBe` expected

  it "contra" $ do
    let input =
          ProofState
            { goal = Falsum,
              assumptions = [Atom "x", Not (Atom "x")],
              subProofs = [],
              tactics = []
            }
        actual = prove (Contra (Atom "x") (Not (Atom "x"))) input
        expected =
          ProofState
            { goal = Falsum,
              assumptions = [Falsum, Atom "x", Not (Atom "x")],
              subProofs = [],
              tactics = [Contra (Atom "x") (Not (Atom "x"))]
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
