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
  = Assume Prop -- assume
  | Suppose Prop -- suppose
  | AndIntro Prop -- andI
  | AndElimLeft Prop -- andEL
  | AndElimRight Prop -- andER
  | OrIntro Prop -- orI
  | OrElim Prop Prop Prop -- orE
  | ImpIntro Prop -- impI
  | ImpElim Prop Prop -- impE
  | Dn Prop -- dn
  | Contra Prop Prop -- contra
  | Done -- done
  deriving (Eq)

instance Show Tactic where
  show (Assume p) = "assume " ++ show p
  show (Suppose p) = "suppose " ++ show p
  show (AndIntro p1) = "andI " ++ show p1
  show (AndElimLeft p) = "andEL " ++ show p
  show (AndElimRight p) = "andER " ++ show p
  show (OrIntro p) = "orI " ++ show p
  show (OrElim p1 p2 p3) = "orE " ++ show p1 ++ " " ++ show p2 ++ " " ++ show p3
  show (ImpIntro p1) = "impI " ++ show p1
  show (ImpElim p1 p2) = "impE " ++ show p1 ++ " " ++ show p2
  show (Dn p) = "dn " ++ show p
  show (Contra p1 p2) = "contra " ++ show p1 ++ " " ++ show p2
  show Done = "done"

data ProofState = ProofState
  { goal :: Prop,
    assumptions :: [Prop],
    subProofs :: [ProofState],
    tactics :: [Tactic]
  }
  deriving (Eq)

printState :: Int -> ProofState -> String
printState _ (ProofState _ _ _ (Done : _)) = ""
printState indent (ProofState g a s t) =
  unlines $
    [ replicate indent ' ' ++ "Goal: " ++ show g,
      replicate indent ' ' ++ "Assumptions: " ++ show a,
      replicate indent ' ' ++ "Tactics: " ++ show t
    ]
      ++ map (printState (indent + 2)) s

instance Show ProofState where
  show = printState 0
