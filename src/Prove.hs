module Prove where

import Types

prove :: Tactic -> ProofState -> ProofState
prove tactic state
  | all isProved (subProofs state) = update tactic state
  | otherwise = state {subProofs = prove_ tactic (subProofs state)}

prove_ :: Tactic -> [ProofState] -> [ProofState]
prove_ _ [] = []
prove_ tactic (x : xs)
  | not (isProved x) = prove tactic x : xs
  | otherwise = x : prove_ tactic xs

update :: Tactic -> ProofState -> ProofState
update tactic state =
  case tactic of
    Assume p -> assumeRule p state
    AndIntro -> andIntroRule state
    AndElimLeft pq -> andElimLeftRule pq state
    AndElimRight pq -> andElimRightRule pq state
    OrIntroLeft -> orIntroLeftRule state
    OrIntroRight -> orIntroRightRule state
    OrElim pq -> orElimRule pq state
    ImpIntro -> impIntroRule state
    ImpElim p pq -> impElimRule p pq state
    DnIntro -> dnIntroRule state
    DnElim p -> dnElimRule p state
    Contra p np -> contraRule p np state
    Done -> doneRule state

assumeRule :: Prop -> ProofState -> ProofState
assumeRule p state =
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
          tactics = Assume p : tactics state
        }

andIntroRule :: ProofState -> ProofState
andIntroRule state@(ProofState (And p q) _ _ _) =
  let subProofLeft =
        state
          { goal = p,
            assumptions = assumptions state,
            subProofs = [],
            tactics = []
          }
      subProofRight =
        state
          { goal = q,
            assumptions = assumptions state,
            subProofs = [],
            tactics = []
          }
   in state
        { assumptions = And p q : assumptions state,
          subProofs = [subProofLeft, subProofRight],
          tactics = AndIntro : tactics state
        }
andIntroRule _ =
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

orIntroLeftRule :: ProofState -> ProofState
orIntroLeftRule state@(ProofState (Or p q) _ _ _) =
  state
    { goal = p,
      assumptions = Or p q : assumptions state,
      tactics = OrIntroLeft : tactics state
    }
orIntroLeftRule _ =
  error "Or Introduction Left must be applied to a disjunction"

orIntroRightRule :: ProofState -> ProofState
orIntroRightRule state@(ProofState (Or p q) _ _ _) =
  state
    { goal = q,
      assumptions = Or p q : assumptions state,
      tactics = OrIntroRight : tactics state
    }
orIntroRightRule _ =
  error "Or Introduction Right must be applied to a disjunction"

orElimRule :: Prop -> ProofState -> ProofState
orElimRule (Or p q) state
  | isInAssumptions (Or p q) state =
      let subProofLeft =
            state
              { goal = p,
                assumptions = assumptions state,
                subProofs = [],
                tactics = []
              }
          subProofRight =
            state
              { goal = q,
                assumptions = assumptions state,
                subProofs = [],
                tactics = []
              }
       in state
            { assumptions = Or p q : assumptions state,
              subProofs = [subProofLeft, subProofRight],
              tactics = OrElim (Or p q) : tactics state
            }
  | otherwise =
      error "Or Elimination is not valid in the current context"
orElimRule _ _ =
  error "Or Elimination must be applied to a disjunction"

impIntroRule :: ProofState -> ProofState
impIntroRule state@(ProofState (Imp p q) _ _ _) =
  state
    { goal = q,
      assumptions = p : assumptions state,
      tactics = ImpIntro : tactics state
    }
impIntroRule _ =
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

dnIntroRule :: ProofState -> ProofState
dnIntroRule state@(ProofState (Not (Not p)) _ _ _) =
  state
    { goal = p,
      tactics = DnIntro : tactics state
    }
dnIntroRule _ =
  error "Double Negation Introduction must be applied to a negation"

dnElimRule :: Prop -> ProofState -> ProofState
dnElimRule p state
  | isInAssumptions (Not (Not p)) state =
      state
        { assumptions = p : assumptions state,
          tactics = DnElim p : tactics state
        }
  | otherwise =
      error "Double Negation Elimination is not valid in the current context"

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
      state {tactics = Done : tactics state}
  | otherwise =
      error "Proof is not complete, assumptions do not match the goal"

-- Helper functions

isInAssumptions :: Prop -> ProofState -> Bool
isInAssumptions prop state =
  prop `elem` assumptions state

isProved :: ProofState -> Bool
isProved state =
  case tactics state of
    [] -> False
    (Done : _) -> True
    _ -> False
