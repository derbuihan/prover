module Proof where

import Types

update :: Tactic -> ProofState -> ProofState
update tactic state
  | all isProved (subProofs state) = prove tactic state
  | otherwise =
      state
        { subProofs = update_ tactic (subProofs state)
        }

update_ :: Tactic -> [ProofState] -> [ProofState]
update_ _ [] = []
update_ tactic (x : xs)
  | not (isProved x) = update tactic x : xs
  | otherwise = x : update_ tactic xs

prove :: Tactic -> ProofState -> ProofState
prove (Assume x) state = proveAssume x state
prove (Suppose x) state = proveSuppose x state
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
proveAssume p state@(ProofState (Imp p_ q) _ _ _)
  | p == p_ =
      state
        { goal = q,
          assumptions = p : assumptions state,
          tactics = Assume p : tactics state
        }
  | otherwise =
      error "Assumption must be an implication"
proveAssume _ _ =
  error "Assumption must be an implication"

proveSuppose :: Prop -> ProofState -> ProofState
proveSuppose x state =
  let subProof =
        state
          { goal = Falsum,
            assumptions = x : assumptions state,
            subProofs = [],
            tactics = []
          }
   in state
        { assumptions = Not x : assumptions state,
          subProofs = subProof : subProofs state,
          tactics = Suppose x : tactics state
        }

proveAndIntro :: Prop -> ProofState -> ProofState
proveAndIntro (And p q) state
  | isInAssumptions p state && isInAssumptions q state =
      state
        { assumptions = And p q : assumptions state,
          tactics = AndIntro (And p q) : tactics state
        }
  | otherwise =
      error "And Introduction is not valid in the current context"
proveAndIntro _ _ =
  error "And Introduction must be applied to a conjunction"

proveAndElimLeft :: Prop -> ProofState -> ProofState
proveAndElimLeft (And p q) state
  | isInAssumptions (And p q) state =
      state
        { assumptions = p : assumptions state,
          tactics = AndElimLeft (And p q) : tactics state
        }
  | otherwise =
      error "And Elimination Left is not valid in the current context"
proveAndElimLeft _ _ =
  error "And Elimination Left must be applied to a conjunction"

proveAndElimRight :: Prop -> ProofState -> ProofState
proveAndElimRight (And p q) state
  | isInAssumptions (And p q) state =
      state
        { assumptions = q : assumptions state,
          tactics = AndElimRight (And p q) : tactics state
        }
  | otherwise =
      error "And Elimination Right is not valid in the current context"
proveAndElimRight _ _ =
  error "And Elimination Right must be applied to a conjunction"

proveOrIntro :: Prop -> ProofState -> ProofState
proveOrIntro (Or p q) state
  | isInAssumptions p state || isInAssumptions q state =
      state
        { assumptions = Or p q : assumptions state,
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
      state
        { assumptions = r : assumptions state,
          tactics = OrElim (Or p q) (Imp p_ r) (Imp q_ r_) : tactics state
        }
  | otherwise =
      error "Or Elimination is not valid in the current context"
proveOrElim _ _ _ _ =
  error "Or Elimination must be applied to a disjunction and two implications"

proveImpIntro :: Prop -> ProofState -> ProofState
proveImpIntro (Imp p q) state
  | isInAssumptions p state && isInAssumptions q state =
      state
        { assumptions = Imp p q : assumptions state,
          tactics = ImpIntro (Imp p q) : tactics state
        }
  | otherwise =
      error "Implication Introduction is not valid in the current context"
proveImpIntro _ _ =
  error "Implication Introduction must be applied to an implication"

proveImpElim :: Prop -> Prop -> ProofState -> ProofState
proveImpElim p (Imp p_ q) state
  | isInAssumptions p state && isInAssumptions (Imp p_ q) state && p == p_ =
      state
        { assumptions = q : assumptions state,
          tactics = ImpElim p (Imp p_ q) : tactics state
        }
  | otherwise =
      error "Implication Elimination is not valid in the current context"
proveImpElim _ _ _ =
  error "Implication Elimination must be applied to an implication and a proposition"

proveDn :: Prop -> ProofState -> ProofState
proveDn (Not (Not p)) state
  | isInAssumptions (Not (Not p)) state =
      state
        { assumptions = p : assumptions state,
          tactics = Dn (Not (Not p)) : tactics state
        }
  | otherwise =
      error "Double Negation is not valid in the current context"
proveDn _ _ =
  error "Double Negation must be applied to a double negation"

proveContra :: Prop -> Prop -> ProofState -> ProofState
proveContra p (Not p_) state
  | isInAssumptions p state && isInAssumptions (Not p_) state && p == p_ && goal state == Falsum =
      state
        { assumptions = Falsum : assumptions state,
          tactics = Contra p (Not p_) : tactics state
        }
  | otherwise =
      error "Contradiction is not valid in the current context"
proveContra _ _ _ =
  error "Contradiction must be applied to a proposition and its negation"

proveDone :: ProofState -> ProofState
proveDone state
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
