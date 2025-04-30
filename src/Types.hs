module Types where

data Token
  = TVar String -- x, y
  | TNot -- !
  | TAnd -- &
  | TOr
  | TImply -- ->
  | TIff -- <->
  | TLParen -- (
  | TRParen -- )
  | TEOF -- end of file
  | TAssum -- assum
  | TModusPonens -- modus ponens
  | TDoubleNegationElim -- double negation elimination
  | TApply -- apply
  | TDone -- done
  deriving (Eq, Show)

data Prop
  = Var String -- x, y
  | Not Prop -- !x
  | And Prop Prop -- x & y
  | Or Prop Prop -- x | y
  | Imply Prop Prop -- x -> y
  | Iff Prop Prop -- x <-> y
  deriving (Eq)

instance Show Prop where
  show (Var s) = s
  show (Not p) = "!" ++ show p
  show (And p1 p2) = "(" ++ show p1 ++ " & " ++ show p2 ++ ")"
  show (Or p1 p2) = "(" ++ show p1 ++ " | " ++ show p2 ++ ")"
  show (Imply p1 p2) = "(" ++ show p1 ++ " -> " ++ show p2 ++ ")"
  show (Iff p1 p2) = "(" ++ show p1 ++ " <-> " ++ show p2 ++ ")"

data Tactic
  = Assum Prop
  | ModusPonens Prop Prop
  | DoubleNegationIntro Prop
  | DoubleNegationElim Prop
  | ConditionalProof Prop Prop
  | AndIntro Prop Prop
  | AndElimLeft Prop
  | AndElimRight Prop
  | OrIntroLeft Prop
  | OrIntroRight Prop
  | OrElim Prop Prop Prop Prop
  | Contradiction Prop Prop
  | Done
  deriving (Eq)

instance Show Tactic where
  show (Assum p) = "assum " ++ show p
  show (ModusPonens p q) = "mpp " ++ show p ++ " " ++ show q
  show (DoubleNegationElim p) = "dn " ++ show p
  show Done = "done"
  show _ = "other tactic"

data ProofState = ProofState
  { goal :: Prop,
    assumptions :: [Prop],
    tactics :: [Tactic]
  }
  deriving (Eq)

instance Show ProofState where
  show (ProofState g a t) =
    unlines $
      [ "    Goal: " ++ show g,
        "    Assumptions: " ++ show a,
        "    Tactics: " ++ show t
      ]
