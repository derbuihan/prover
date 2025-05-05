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
  it "assume p for q" $ do
    let input =
          ProofState
            { goal = Imp (Atom "x") (Atom "y"),
              assumptions = [],
              subProofs = [],
              tactics = [],
              completed = False
            }
        actual = prove (Assume (Atom "x") (Atom "y")) input
        expected =
          ProofState
            { goal = Imp (Atom "x") (Atom "y"),
              assumptions = [Imp (Atom "x") (Atom "y")],
              subProofs =
                [ ProofState
                    { goal = Atom "y",
                      assumptions = [Atom "x"],
                      subProofs = [],
                      tactics = [],
                      completed = False
                    }
                ],
              tactics = [Assume (Atom "x") (Atom "y")],
              completed = False
            }
    actual `shouldBe` expected

  it "suppose p" $ do
    let input =
          ProofState
            { goal = Atom "x",
              assumptions = [],
              subProofs = [],
              tactics = [],
              completed = False
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
                      tactics = [],
                      completed = False
                    }
                ],
              tactics = [Suppose (Atom "x")],
              completed = False
            }
    actual `shouldBe` expected

  it "andI p&q" $ do
    let input =
          ProofState
            { goal = Atom "x",
              assumptions = [Atom "x", Atom "y"],
              subProofs = [],
              tactics = [],
              completed = False
            }
        actual = prove (AndIntro (And (Atom "x") (Atom "y"))) input
        expected =
          ProofState
            { goal = Atom "x",
              assumptions = [And (Atom "x") (Atom "y"), Atom "x", Atom "y"],
              subProofs = [],
              tactics = [AndIntro (And (Atom "x") (Atom "y"))],
              completed = False
            }
    actual `shouldBe` expected

  it "andEL p&q" $ do
    let input =
          ProofState
            { goal = Atom "x",
              assumptions = [And (Atom "x") (Atom "y")],
              subProofs = [],
              tactics = [],
              completed = False
            }
        actual = prove (AndElimLeft (And (Atom "x") (Atom "y"))) input
        expected =
          ProofState
            { goal = Atom "x",
              assumptions = [Atom "x", And (Atom "x") (Atom "y")],
              subProofs = [],
              tactics = [AndElimLeft (And (Atom "x") (Atom "y"))],
              completed = False
            }
    actual `shouldBe` expected

  it "andER p&q" $ do
    let input =
          ProofState
            { goal = Atom "x",
              assumptions = [And (Atom "x") (Atom "y")],
              subProofs = [],
              tactics = [],
              completed = False
            }
        actual = prove (AndElimRight (And (Atom "x") (Atom "y"))) input
        expected =
          ProofState
            { goal = Atom "x",
              assumptions = [Atom "y", And (Atom "x") (Atom "y")],
              subProofs = [],
              tactics = [AndElimRight (And (Atom "x") (Atom "y"))],
              completed = False
            }
    actual `shouldBe` expected

  it "orI p|q" $ do
    let input =
          ProofState
            { goal = Atom "x",
              assumptions = [Atom "x"],
              subProofs = [],
              tactics = [],
              completed = False
            }
        actual = prove (OrIntro (Or (Atom "x") (Atom "y"))) input
        expected =
          ProofState
            { goal = Atom "x",
              assumptions = [Or (Atom "x") (Atom "y"), Atom "x"],
              subProofs = [],
              tactics = [OrIntro (Or (Atom "x") (Atom "y"))],
              completed = False
            }
    actual `shouldBe` expected

  it "orE p|q for r" $ do
    let input =
          ProofState
            { goal = Atom "z",
              assumptions = [Or (Atom "x") (Atom "y")],
              subProofs = [],
              tactics = [],
              completed = False
            }
        actual = prove (OrElim (Or (Atom "x") (Atom "y")) (Atom "z")) input
        expected =
          ProofState
            { goal = Atom "z",
              assumptions = [Atom "z", Or (Atom "x") (Atom "y")],
              subProofs =
                [ ProofState
                    { goal = Atom "z",
                      assumptions = [Atom "x", Or (Atom "x") (Atom "y")],
                      subProofs = [],
                      tactics = [],
                      completed = False
                    },
                  ProofState
                    { goal = Atom "z",
                      assumptions = [Atom "y", Or (Atom "x") (Atom "y")],
                      subProofs = [],
                      tactics = [],
                      completed = False
                    }
                ],
              tactics = [OrElim (Or (Atom "x") (Atom "y")) (Atom "z")],
              completed = False
            }
    actual `shouldBe` expected

  it "impI p->q" $ do
    let input =
          ProofState
            { goal = Atom "z",
              assumptions = [Atom "x", Atom "y"],
              subProofs = [],
              tactics = [],
              completed = False
            }
        actual = prove (ImpIntro (Imp (Atom "x") (Atom "y"))) input
        expected =
          ProofState
            { goal = Atom "z",
              assumptions = [Imp (Atom "x") (Atom "y"), Atom "x", Atom "y"],
              subProofs = [],
              tactics = [ImpIntro (Imp (Atom "x") (Atom "y"))],
              completed = False
            }
    actual `shouldBe` expected

  it "impE p p->q" $ do
    let input =
          ProofState
            { goal = Atom "y",
              assumptions = [Atom "x", Imp (Atom "x") (Atom "y")],
              subProofs = [],
              tactics = [],
              completed = False
            }
        actual = prove (ImpElim (Atom "x") (Imp (Atom "x") (Atom "y"))) input
        expected =
          ProofState
            { goal = Atom "y",
              assumptions = [Atom "y", Atom "x", Imp (Atom "x") (Atom "y")],
              subProofs = [],
              tactics = [ImpElim (Atom "x") (Imp (Atom "x") (Atom "y"))],
              completed = False
            }
    actual `shouldBe` expected

  it "dn p" $ do
    let input =
          ProofState
            { goal = Atom "x",
              assumptions = [Not (Not (Atom "x"))],
              subProofs = [],
              tactics = [],
              completed = False
            }
        actual = prove (Dn (Atom "x")) input
        expected =
          ProofState
            { goal = Atom "x",
              assumptions = [Atom "x", Not (Not (Atom "x"))],
              subProofs = [],
              tactics = [Dn (Atom "x")],
              completed = False
            }
    actual `shouldBe` expected

  it "contra p !p" $ do
    let input =
          ProofState
            { goal = Falsum,
              assumptions = [Atom "x", Not (Atom "x")],
              subProofs = [],
              tactics = [],
              completed = False
            }
        actual = prove (Contra (Atom "x") (Not (Atom "x"))) input
        expected =
          ProofState
            { goal = Falsum,
              assumptions = [Falsum, Atom "x", Not (Atom "x")],
              subProofs = [],
              tactics = [Contra (Atom "x") (Not (Atom "x"))],
              completed = False
            }
    actual `shouldBe` expected

  it "done" $ do
    let input =
          ProofState
            { goal = Atom "x",
              assumptions = [Atom "x"],
              subProofs = [],
              tactics = [],
              completed = False
            }
        actual = prove Done input
        expected =
          ProofState
            { goal = Atom "x",
              assumptions = [Atom "x"],
              subProofs = [],
              tactics = [Done],
              completed = True
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
              tactics = [],
              completed = True
            }
        actual = isInAssumptions (Atom "x") input
        expected = True
    actual `shouldBe` expected
