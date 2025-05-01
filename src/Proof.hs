module Proof where

import Types

prove :: Tactic -> ProofState -> ProofState
prove (Assume x) state = proveAssume x state
prove (AndIntro pq) state = proveAndIntro pq state
prove (AndElimLeft p) state = proveAndElimLeft p state
prove (AndElimRight q) state = proveAndElimRight q state
prove (OrIntro pq) state = proveOrIntro pq state
prove (OrElim pq pr qr) state = proveOrElim pq pr qr state
prove (ImpIntro pq) state = proveImpIntro pq state
prove (ImpElim p pq) state = proveImpElim p pq state
prove (Dn p) state = proveDn p state
prove (Contra p np) state = proveContra p np state
prove Done state = proveDone state

proveAssume :: Prop -> ProofState -> ProofState
proveAssume x state =
  ProofState
    { goal = goal state,
      assumptions = x : assumptions state,
      tactics = Assume x : tactics state
    }

proveAndIntro :: Prop -> ProofState -> ProofState
proveAndIntro (And p q) state
  | isInAssumptions p state && isInAssumptions q state =
      ProofState
        { goal = goal state,
          assumptions = And p q : assumptions state,
          tactics = AndIntro (And p q) : tactics state
        }
  | otherwise =
      error "And Introduction is not valid in the current context"
proveAndIntro _ _ =
  error "And Introduction must be applied to a conjunction"

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
  error "And Elimination Left must be applied to a conjunction"

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
  error "And Elimination Right must be applied to a conjunction"

proveOrIntro :: Prop -> ProofState -> ProofState
proveOrIntro (Or p q) state
  | isInAssumptions p state || isInAssumptions q state =
      ProofState
        { goal = goal state,
          assumptions = Or p q : assumptions state,
          tactics = OrIntro (Or p q) : tactics state
        }
  | otherwise =
      error "Or Introduction is not valid in the current context"
proveOrIntro _ _ =
  error "Or Introduction must be applied to a disjunction"

proveOrElim :: Prop -> Prop -> Prop -> ProofState -> ProofState
proveOrElim (Or p q) (Imp p_ r) (Imp q_ r_) state
  | isInAssumptions (Or p q) state
      && isInAssumptions (Imp p_ r) state
      && isInAssumptions (Imp q_ r_) state
      && p == p_
      && q == q_
      && r == r_ =
      ProofState
        { goal = goal state,
          assumptions = r : assumptions state,
          tactics = OrElim (Or p q) (Imp p_ r) (Imp q_ r_) : tactics state
        }
  | otherwise =
      error "Or Elimination is not valid in the current context"
proveOrElim _ _ _ _ =
  error "Or Elimination must be applied to a disjunction and two implications"

proveImpIntro :: Prop -> ProofState -> ProofState
proveImpIntro (Imp p q) state
  | isInAssumptions p state && isInAssumptions q state =
      ProofState
        { goal = goal state,
          assumptions = Imp p q : assumptions state,
          tactics = ImpIntro (Imp p q) : tactics state
        }
  | otherwise =
      error "Implication Introduction is not valid in the current context"
proveImpIntro _ _ =
  error "Implication Introduction must be applied to an implication"

proveImpElim :: Prop -> Prop -> ProofState -> ProofState
proveImpElim p (Imp p_ q) state
  | isInAssumptions p state && isInAssumptions (Imp p_ q) state && p == p_ =
      ProofState
        { goal = goal state,
          assumptions = q : assumptions state,
          tactics = ImpElim p (Imp p_ q) : tactics state
        }
  | otherwise =
      error "Implication Elimination is not valid in the current context"
proveImpElim _ _ _ =
  error "Implication Elimination must be applied to an implication and a proposition"

proveDn :: Prop -> ProofState -> ProofState
proveDn (Not (Not p)) state
  | isInAssumptions (Not (Not p)) state =
      ProofState
        { goal = goal state,
          assumptions = p : assumptions state,
          tactics = Dn (Not (Not p)) : tactics state
        }
  | otherwise =
      error "Double Negation is not valid in the current context"
proveDn _ _ =
  error "Double Negation must be applied to a double negation"

proveContra :: Prop -> Prop -> ProofState -> ProofState
proveContra p (Not p_) state
  | isInAssumptions p state && isInAssumptions (Not p_) state && p == p_ =
      ProofState
        { goal = goal state,
          assumptions = [],
          tactics = Contra p (Not p_) : tactics state
        }
  | otherwise =
      error "Contradiction is not valid in the current context"
proveContra _ _ _ =
  error "Contradiction must be applied to a proposition and its negation"

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
