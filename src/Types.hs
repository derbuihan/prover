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
  | TAndIntro -- andI
  | TAndElimLeft -- andEL
  | TAndElimRight -- andER
  | TOrIntroLeft -- orIL
  | TOrIntroRight -- orIR
  | TOrElim -- orE
  | TImpIntro -- impI
  | TImpElim -- impE
  | TDnIntro -- dnI
  | TDnElim -- dnE
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
  | AndIntro -- andI
  | AndElimLeft Prop -- andEL
  | AndElimRight Prop -- andER
  | OrIntroLeft -- orIL
  | OrIntroRight -- orIR
  | OrElim Prop -- orE
  | ImpIntro -- impI
  | ImpElim Prop Prop -- impE
  | DnIntro -- dnI
  | DnElim Prop -- dnE
  | Contra Prop Prop -- contra
  | Done -- done
  deriving (Eq)

instance Show Tactic where
  show (Assume p) = "assume " ++ show p
  show AndIntro = "andI"
  show (AndElimLeft p) = "andEL " ++ show p
  show (AndElimRight p) = "andER " ++ show p
  show OrIntroLeft = "orIL"
  show OrIntroRight = "orIR"
  show (OrElim p) = "orE " ++ show p
  show ImpIntro = "impI"
  show (ImpElim p1 p2) = "impE " ++ show p1 ++ " " ++ show p2
  show DnIntro = "dnI"
  show (DnElim p) = "dnE " ++ show p
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
