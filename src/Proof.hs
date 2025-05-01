module Proof where

import Types

-- data Tactic
--   = Assume Prop -- Assumption
--   | AndIntro Prop Prop -- And Introduction
--   | AndElimLeft Prop -- And Elimination Left
--   | AndElimRight Prop -- And Elimination Right
--   | OrIntroLeft Prop -- Or Introduction Left
--   | OrIntroRight Prop -- Or Introduction Right
--   | OrElim Prop Prop Prop -- Or Elimination
--   | ImpIntro Prop Prop -- Implication Introduction
--   | ImpElim Prop Prop -- Implication Elimination
--   | DnIntro Prop -- Double Negation Introduction
--   | DnElim Prop -- Double Negation Elimination
--   | Contra Prop Prop -- Contradiction
--   | Done -- Done
--   deriving (Eq)

prove :: Tactic -> ProofState -> ProofState
prove (Assume p) state = proveAssum p state
prove (AndIntro p q) state = proveAndIntro p q state
prove (AndElimLeft p) state = proveAndElimLeft p state
prove (AndElimRight q) state = proveAndElimRight q state
-- prove (OrIntro p) state = proveOrIntro p q state
-- prove (OrElim p q r) state = proveOrElim p q r state
-- prove (ImpIntro p q) state = proveImpIntro p q state
-- prove (ImpElim pq p) state = proveImpElim pq p state
-- prove (DnIntro p) state = proveDnIntro p state
-- prove (DnElim p) state = proveDnElim p state
-- prove (Contra p q) state = proveContra p q state
prove Done state = proveDone state
prove _ _ =
  error "Invalid tactic"

proveAssum :: Prop -> ProofState -> ProofState
proveAssum prop state@ProofState {goal = Imp left right}
  | prop == left =
      ProofState
        { goal = right,
          assumptions = prop : assumptions state,
          tactics = Assume prop : tactics state
        }
  | otherwise =
      error "Assumption does not match the left side of the implication"
proveAssum _ _ =
  error "Assumption is not valid in the current context"

proveAndIntro :: Prop -> Prop -> ProofState -> ProofState
proveAndIntro p q state
  | isInAssumptions p state && isInAssumptions q state =
      ProofState
        { goal = goal state,
          assumptions = And p q : assumptions state,
          tactics = AndIntro p q : tactics state
        }
  | otherwise =
      error "And Introduction is not valid in the current context"

proveAndElimLeft :: Prop -> ProofState -> ProofState
proveAndElimLeft (And p q) state
  | isInAssumptions (And p q) state =
      ProofState
        { goal = goal state,
          assumptions = p : assumptions state,
          tactics = AndElimLeft (And p q) : tactics state
        }
  | otherwise =
      error "And Elimination Left is not valid in the current context"
proveAndElimLeft _ _ =
  error "And Elimination Left is not valid in the current context"

proveAndElimRight :: Prop -> ProofState -> ProofState
proveAndElimRight (And p q) state
  | isInAssumptions (And p q) state =
      ProofState
        { goal = goal state,
          assumptions = q : assumptions state,
          tactics = AndElimRight (And p q) : tactics state
        }
  | otherwise =
      error "And Elimination Right is not valid in the current context"
proveAndElimRight _ _ =
  error "And Elimination Right is not valid in the current context"

proveDone :: ProofState -> ProofState
proveDone state
  | isInAssumptions (goal state) state =
      ProofState
        { goal = goal state,
          assumptions = assumptions state,
          tactics = Done : tactics state
        }
  | otherwise =
      error "Proof is not complete, assumptions do not match the goal"

isInAssumptions :: Prop -> ProofState -> Bool
isInAssumptions prop state =
  prop `elem` assumptions state
