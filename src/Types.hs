module Types where

data Token
  = TAtom String -- x, y
  | TNot -- !
  | TAnd -- &
  | TOr
  | TImp -- ->
  | TIff -- <->
  | TLParen -- (
  | TRParen -- )
  | TEOF -- end of file
  | TAssum -- assumption
  | TAndIntro -- and introduction
  | TAndElimLeft -- and elimination left
  | TAndElimRight -- and elimination right
  | TOrIntroLeft -- or introduction left
  | TOrIntroRight -- or introduction right
  | TOrElim -- or elimination
  | TImpIntro -- implication introduction
  | TImpElim -- implication elimination
  | TDnIntro -- double negation introduction
  | TDnElim -- double negation elimination
  | TContra -- contradiction
  | TDone -- done
  deriving (Eq, Show)

data Prop
  = Atom String -- x, y
  | Not Prop -- !x
  | And Prop Prop -- x & y
  | Or Prop Prop -- x | y
  | Imp Prop Prop -- x -> y
  | Iff Prop Prop -- x <-> y
  deriving (Eq)

instance Show Prop where
  show (Atom s) = s
  show (Not p) = "!" ++ show p
  show (And p1 p2) = "(" ++ show p1 ++ " & " ++ show p2 ++ ")"
  show (Or p1 p2) = "(" ++ show p1 ++ " | " ++ show p2 ++ ")"
  show (Imp p1 p2) = "(" ++ show p1 ++ " -> " ++ show p2 ++ ")"
  show (Iff p1 p2) = "(" ++ show p1 ++ " <-> " ++ show p2 ++ ")"

data Tactic
  = Assume Prop -- Assumption
  | AndIntro Prop Prop -- And Introduction
  | AndElimLeft Prop -- And Elimination Left
  | AndElimRight Prop -- And Elimination Right
  | OrIntroLeft Prop -- Or Introduction Left
  | OrIntroRight Prop -- Or Introduction Right
  | OrElim Prop Prop Prop -- Or Elimination
  | ImpIntro Prop Prop -- Implication Introduction
  | ImpElim Prop Prop -- Implication Elimination
  | DnIntro Prop -- Double Negation Introduction
  | DnElim Prop -- Double Negation Elimination
  | Contra Prop Prop -- Contradiction
  | Done -- Done
  deriving (Eq)

instance Show Tactic where
  show (Assume p) = "assume " ++ show p
  show (AndIntro p1 p2) = "andI " ++ show p1 ++ " " ++ show p2
  show (AndElimLeft p) = "andEL " ++ show p
  show (AndElimRight p) = "andER " ++ show p
  show (OrIntroLeft p) = "orIL " ++ show p
  show (OrIntroRight p) = "orIR " ++ show p
  show (OrElim p1 p2 p3) = "orE " ++ show p1 ++ " " ++ show p2 ++ " " ++ show p3
  show (ImpIntro p1 p2) = "impI " ++ show p1 ++ " " ++ show p2
  show (ImpElim p1 p2) = "impE " ++ show p1 ++ " " ++ show p2
  show (DnIntro p) = "dnI " ++ show p
  show (DnElim p) = "dnE " ++ show p
  show (Contra p1 p2) = "contra " ++ show p1 ++ " " ++ show p2
  show Done = "done"

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
