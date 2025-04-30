module Proof where

import Types

prove :: Tactic -> ProofState -> ProofState
prove (Assum prop) state = proveAssum prop state
prove (ModusPonens pq p) state = proveModusPonens pq p state
prove (DoubleNegationElim prop) state = proveDoubleNegationElim prop state
prove Done state = proveDone state
prove _ _ = error "Invalid tactic"

proveAssum :: Prop -> ProofState -> ProofState
proveAssum prop state@ProofState {goal = Imply left right}
  | prop == left =
      ProofState
        { goal = right,
          assumptions = prop : assumptions state,
          tactics = Assum prop : tactics state
        }
  | otherwise =
      error "Assumption does not match the left side of the implication"
proveAssum _ _ =
  error "Assumption is not valid in the current context"

proveModusPonens :: Prop -> Prop -> ProofState -> ProofState
proveModusPonens (Imply p q) p_ state
  | isInAssumptions (Imply p q) state && isInAssumptions p_ state && p == p_ =
      ProofState
        { goal = goal state,
          assumptions = q : assumptions state,
          tactics = ModusPonens (Imply p q) p_ : tactics state
        }
  | otherwise =
      error "Modus Ponens is not valid in the current context"
proveModusPonens _ _ _ =
  error "Modus Ponens is not valid in the current context"

proveDoubleNegationElim :: Prop -> ProofState -> ProofState
proveDoubleNegationElim (Not (Not p)) state
  | isInAssumptions (Not (Not p)) state =
      ProofState
        { goal = goal state,
          assumptions = p : assumptions state,
          tactics = DoubleNegationElim (Not (Not p)) : tactics state
        }
  | otherwise =
      error "Double negation elimination is not valid in the current context"
proveDoubleNegationElim _ _ =
  error "Double negation elimination is not valid in the current context"

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
