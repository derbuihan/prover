module Prove where

import Convert
import Types

prove :: Tactic -> ProofState -> ProofState
prove tactic state
  | all completed (subProofs state) = update tactic state
  | otherwise = state {subProofs = prove_ tactic (subProofs state)}

prove_ :: Tactic -> [ProofState] -> [ProofState]
prove_ _ [] = []
prove_ tactic (x : xs)
  | not (completed x) = prove tactic x : xs
  | otherwise = x : prove_ tactic xs

update :: Tactic -> ProofState -> ProofState
update tactic state =
  case tactic of
    Assume p q -> assumeRule p q state
    Suppose p -> supposeRule p state
    AndIntro pq -> andIntroRule pq state
    AndElimLeft pq -> andElimLeftRule pq state
    AndElimRight pq -> andElimRightRule pq state
    OrIntro pq -> orIntroRule pq state
    OrElim pq r -> orElimRule pq r state
    ImpIntro pq -> impIntroRule pq state
    ImpElim p pq -> impElimRule p pq state
    Dn p -> dnRule p state
    Contra p np -> contraRule p np state
    Done -> doneRule state
    Fix t -> fixRule t state
    ForallIntro t p -> forallIntroRule t p state
    ForallElim t p -> forallElimRule t p state
    -- ExistsIntro t p -> existsIntroRule t p state
    -- ExistsElim t p q -> existsElimRule t p q state
    _ -> error "Invalid tactic"

assumeRule :: Prop -> Prop -> ProofState -> ProofState
assumeRule p q state =
  let subProof =
        state
          { goal = q,
            assumptions = p : assumptions state,
            subProofs = [],
            tactics = []
          }
   in state
        { assumptions = Imp p q : assumptions state,
          subProofs = subProof : subProofs state,
          tactics = Assume p q : tactics state
        }

supposeRule :: Prop -> ProofState -> ProofState
supposeRule p state =
  let subProof =
        state
          { goal = Falsum,
            assumptions = p : assumptions state,
            subProofs = [],
            tactics = []
          }
   in state
        { assumptions = Not p : assumptions state,
          subProofs = subProof : subProofs state,
          tactics = Suppose p : tactics state
        }

andIntroRule :: Prop -> ProofState -> ProofState
andIntroRule (And p q) state
  | isInAssumptions p state && isInAssumptions q state =
      state
        { assumptions = And p q : assumptions state,
          tactics = AndIntro (And p q) : tactics state
        }
  | otherwise =
      error "And Introduction is not valid in the current context"
andIntroRule _ _ =
  error "And Introduction must be applied to a conjunction"

andElimLeftRule :: Prop -> ProofState -> ProofState
andElimLeftRule (And p q) state
  | isInAssumptions (And p q) state =
      state
        { assumptions = p : assumptions state,
          tactics = AndElimLeft (And p q) : tactics state
        }
  | otherwise =
      error "And Elimination Left is not valid in the current context"
andElimLeftRule _ _ =
  error "And Elimination Left must be applied to a conjunction"

andElimRightRule :: Prop -> ProofState -> ProofState
andElimRightRule (And p q) state
  | isInAssumptions (And p q) state =
      state
        { assumptions = q : assumptions state,
          tactics = AndElimRight (And p q) : tactics state
        }
  | otherwise =
      error "And Elimination Right is not valid in the current context"
andElimRightRule _ _ =
  error "And Elimination Right must be applied to a conjunction"

orIntroRule :: Prop -> ProofState -> ProofState
orIntroRule (Or p q) state
  | isInAssumptions p state || isInAssumptions q state =
      state
        { assumptions = Or p q : assumptions state,
          tactics = OrIntro (Or p q) : tactics state
        }
  | otherwise =
      error "Or Introduction is not valid in the current context"
orIntroRule _ _ =
  error "Or Introduction must be applied to a disjunction"

orElimRule :: Prop -> Prop -> ProofState -> ProofState
orElimRule (Or p q) r state
  | isInAssumptions (Or p q) state =
      let subProofLeft =
            state
              { goal = r,
                assumptions = p : assumptions state,
                subProofs = [],
                tactics = []
              }
          subProofRight =
            state
              { goal = r,
                assumptions = q : assumptions state,
                subProofs = [],
                tactics = []
              }
       in state
            { assumptions = r : assumptions state,
              subProofs = [subProofLeft, subProofRight],
              tactics = OrElim (Or p q) r : tactics state
            }
  | otherwise =
      error "Or Elimination is not valid in the current context"
orElimRule _ _ _ =
  error "Or Elimination must be applied to a disjunction"

impIntroRule :: Prop -> ProofState -> ProofState
impIntroRule (Imp p q) state
  | isInAssumptions p state && isInAssumptions q state =
      state
        { assumptions = Imp p q : assumptions state,
          tactics = ImpIntro (Imp p q) : tactics state
        }
  | otherwise =
      error "Implication Introduction is not valid in the current context"
impIntroRule _ _ =
  error "Implication Introduction must be applied to an implication"

impElimRule :: Prop -> Prop -> ProofState -> ProofState
impElimRule p (Imp p_ q) state
  | isInAssumptions p state && isInAssumptions (Imp p_ q) state && p == p_ =
      state
        { assumptions = q : assumptions state,
          tactics = ImpElim p (Imp p_ q) : tactics state
        }
  | otherwise =
      error "Implication Elimination is not valid in the current context"
impElimRule _ _ _ =
  error "Implication Elimination must be applied to an implication and a proposition"

dnRule :: Prop -> ProofState -> ProofState
dnRule p state
  | isInAssumptions (Not (Not p)) state =
      state
        { assumptions = p : assumptions state,
          tactics = Dn p : tactics state
        }
  | otherwise =
      error "Double Negation is not valid in the current context"

contraRule :: Prop -> Prop -> ProofState -> ProofState
contraRule p (Not p_) state
  | isInAssumptions p state && isInAssumptions (Not p_) state && p == p_ && goal state == Falsum =
      state
        { assumptions = Falsum : assumptions state,
          tactics = Contra p (Not p_) : tactics state
        }
  | otherwise =
      error "Contradiction is not valid in the current context"
contraRule _ _ _ =
  error "Contradiction must be applied to a proposition and its negation"

doneRule :: ProofState -> ProofState
doneRule state
  | isInAssumptions (goal state) state =
      state
        { tactics = Done : tactics state,
          completed = True
        }
  | otherwise =
      error "Proof is not complete, assumptions do not match the goal"

fixRule :: Term -> ProofState -> ProofState
fixRule t state = state {fixed = t : fixed state}

--  forallI a p(a)
forallIntroRule :: Term -> Prop -> ProofState -> ProofState
forallIntroRule (Var a) prop state
  | isInAssumptions prop state && isInFixed (Var a) state =
      let convertedProp = convertProp (Forall a prop)
       in state
            { assumptions = convertedProp : assumptions state,
              tactics = ForallIntro (Var a) prop : tactics state
            }
  | otherwise =
      error "Forall Introduction is not valid in the current context"
forallIntroRule _ _ _ =
  error "Forall Introduction must be applied to a proposition with a variable"

-- forallE a forall x. p(x)
forallElimRule :: Term -> Prop -> ProofState -> ProofState
forallElimRule (Var a) (Forall x p) state
  | isInAssumptions (Forall x p) state && isInFixed (Var a) state =
      let p_ = substituteVarProp (Var a) p
       in state
            { assumptions = p_ : assumptions state,
              tactics = ForallElim (Var a) (Forall x p) : tactics state
            }
forallElimRule _ _ _ =
  error "Forall Introduction must be applied to a Forall proposition"

-- existsIntroRule :: Term -> Prop -> ProofState -> ProofState
-- existsElimRule :: Term -> Prop -> Prop -> ProofState -> ProofState

-- Helper functions

isInAssumptions :: Prop -> ProofState -> Bool
isInAssumptions prop state =
  prop `elem` assumptions state

isInFixed :: Term -> ProofState -> Bool
isInFixed term state =
  term `elem` fixed state

substituteVarProp :: Term -> Prop -> Prop
substituteVarProp term = substituteAtLevel term 0
  where
    substituteAtLevel :: Term -> Int -> Prop -> Prop
    substituteAtLevel t level Falsum = Falsum
    substituteAtLevel t level (Atom s ts) = Atom s (map (substituteVarTerm t level) ts)
    substituteAtLevel t level (Not p) = Not (substituteAtLevel t level p)
    substituteAtLevel t level (And p1 p2) = And (substituteAtLevel t level p1) (substituteAtLevel t level p2)
    substituteAtLevel t level (Or p1 p2) = Or (substituteAtLevel t level p1) (substituteAtLevel t level p2)
    substituteAtLevel t level (Imp p1 p2) = Imp (substituteAtLevel t level p1) (substituteAtLevel t level p2)
    substituteAtLevel t level (Iff p1 p2) = Iff (substituteAtLevel t level p1) (substituteAtLevel t level p2)
    substituteAtLevel t level (Forall s p) = Forall s (substituteAtLevel t (level + 1) p)
    substituteAtLevel t level (Exists s p) = Exists s (substituteAtLevel t (level + 1) p)

substituteVarTerm :: Term -> Int -> Term -> Term
substituteVarTerm term level (VarInt i)
  | i == level = term
  | i > level = VarInt (i - 1)
  | otherwise = VarInt i
substituteVarTerm term level (Func s ts) = Func s (map (substituteVarTerm term level) ts)
substituteVarTerm _ _ v@(Var _) = v
