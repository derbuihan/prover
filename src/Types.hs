module Types where

data Token
  = TFalsum -- ⊥, False
  | TAtom String -- x, y
  | TNot -- !
  | TAnd -- &
  | TOr -- \|
  | TImp -- ->
  | TIff -- <->
  | TLParen -- (
  | TRParen -- )
  | TEOF -- end of file
  | TAssume -- assume
  | TFor -- for
  | TSuppose -- suppose
  | TAndIntro -- andI
  | TAndElimLeft -- andEL
  | TAndElimRight -- andER
  | TOrIntro -- orI
  | TOrElim -- orE
  | TImpIntro -- impI
  | TImpElim -- impE
  | TDn -- dn
  | TContra -- contra
  | TDone -- done
  deriving (Eq, Show)

data Prop
  = Falsum -- False, Falsum, ⊥
  | Atom String -- x, y
  | Not Prop -- !x
  | And Prop Prop -- x & y
  | Or Prop Prop -- x | y
  | Imp Prop Prop -- x -> y
  | Iff Prop Prop -- x <-> y
  deriving (Eq)

instance Show Prop where
  show Falsum = "⊥"
  show (Atom s) = s
  show (Not p) = "!" ++ show p
  show (And p1 p2) = "(" ++ show p1 ++ " & " ++ show p2 ++ ")"
  show (Or p1 p2) = "(" ++ show p1 ++ " | " ++ show p2 ++ ")"
  show (Imp p1 p2) = "(" ++ show p1 ++ " -> " ++ show p2 ++ ")"
  show (Iff p1 p2) = "(" ++ show p1 ++ " <-> " ++ show p2 ++ ")"

data Tactic
  = Assume Prop Prop -- assume p for q
  | Suppose Prop -- suppose p
  | AndIntro Prop -- andI
  | AndElimLeft Prop -- andEL
  | AndElimRight Prop -- andER
  | OrIntro Prop -- orI
  | OrElim Prop Prop -- orE
  | ImpIntro Prop -- impI
  | ImpElim Prop Prop -- impE
  | Dn Prop -- dn
  | Contra Prop Prop -- contra
  | Done -- done
  deriving (Eq)

instance Show Tactic where
  show (Assume p q) = "assume " ++ show p ++ " for " ++ show q
  show (Suppose p) = "suppose " ++ show p
  show (AndIntro p) = "andI " ++ show p
  show (AndElimLeft p) = "andEL " ++ show p
  show (AndElimRight p) = "andER " ++ show p
  show (OrIntro p) = "orI " ++ show p
  show (OrElim p q) = "orE " ++ show p ++ " for " ++ show q
  show (ImpIntro p) = "impI " ++ show p
  show (ImpElim p1 p2) = "impE " ++ show p1 ++ " " ++ show p2
  show (Dn p) = "dn " ++ show p
  show (Contra p1 p2) = "contra " ++ show p1 ++ " " ++ show p2
  show Done = "done"

data ProofState = ProofState
  { goal :: Prop,
    assumptions :: [Prop],
    subProofs :: [ProofState],
    tactics :: [Tactic],
    completed :: Bool
  }
  deriving (Eq)

printProofState :: Int -> ProofState -> String
printProofState _ (ProofState _ _ _ _ True) = ""
printProofState level (ProofState g a s t _) =
  unlines $
    [ indent ++ "Goal: " ++ show g,
      indent ++ "Assumptions: " ++ show a,
      indent ++ "Tactics: " ++ show t
    ]
      ++ map (printProofState (level + 1)) s
  where
    indent = replicate (level * 2) ' '

instance Show ProofState where
  show = printProofState 0
