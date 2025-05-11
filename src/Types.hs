module Types where

data Token
  = TFalsum -- ⊥, False
  | TStr String -- x, y
  | TNot -- !
  | TAnd -- &
  | TOr -- \|
  | TImp -- ->
  | TIff -- <->
  | TLParen -- (
  | TRParen -- )
  | TForall -- ∀
  | TExists -- ∃
  | TComma -- ,
  | TDot -- .
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
  | TFix -- fix
  | TForallIntro -- forallI
  | TForallElim -- forallE
  | TExistsIntro -- existsI
  | TExistsElim -- existsE
  deriving (Eq, Show)

data Term
  = Var String -- x, y
  | VarInt Int -- 1, 2
  | Func String [Term] -- f(x, y), g(x)
  deriving (Eq)

instance Show Term where
  show (Var s) = s
  show (VarInt i) = show i
  show (Func s []) = s
  show (Func s ts) = s ++ "(" ++ concatMap show ts ++ ")"

data Prop
  = Falsum -- False, Falsum, ⊥
  | Atom String [Term] -- x, y, p(x), q(x, y)
  | Not Prop -- !p
  | And Prop Prop -- p & q
  | Or Prop Prop -- p | q
  | Imp Prop Prop -- p -> q
  | Iff Prop Prop -- p <-> q
  | Forall String Prop -- ∀x. p
  | Exists String Prop -- ∃x. p

instance Eq Prop where
  Falsum == Falsum = True
  (Atom s1 ts1) == (Atom s2 ts2) = s1 == s2 && ts1 == ts2
  (Not p1) == (Not p2) = p1 == p2
  (And p1 p2) == (And p3 p4) = (p1 == p3 && p2 == p4) || (p1 == p4 && p2 == p3)
  (Or p1 p2) == (Or p3 p4) = (p1 == p3 && p2 == p4) || (p1 == p4 && p2 == p3)
  (Imp p1 p2) == (Imp p3 p4) = (p1 == p3 && p2 == p4) || (p1 == p4 && p2 == p3)
  (Iff p1 p2) == (Iff p3 p4) = (p1 == p3 && p2 == p4) || (p1 == p4 && p2 == p3)
  (Forall _ p1) == (Forall _ p2) = p1 == p2
  (Exists _ p1) == (Exists _ p2) = p1 == p2
  _ == _ = False

instance Show Prop where
  show Falsum = "⊥"
  show (Atom s []) = s
  show (Atom s ts) = s ++ "(" ++ concatMap show ts ++ ")"
  show (Not p) = "!" ++ show p
  show (And p1 p2) = "(" ++ show p1 ++ " & " ++ show p2 ++ ")"
  show (Or p1 p2) = "(" ++ show p1 ++ " | " ++ show p2 ++ ")"
  show (Imp p1 p2) = "(" ++ show p1 ++ " -> " ++ show p2 ++ ")"
  show (Iff p1 p2) = "(" ++ show p1 ++ " <-> " ++ show p2 ++ ")"
  show (Forall s p) = "∀" ++ s ++ ". " ++ show p
  show (Exists s p) = "∃" ++ s ++ ". " ++ show p

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
  | Fix Term -- fix
  | ForallIntro Term Prop -- forallI
  | ForallElim Term Prop -- forallE
  | ExistsIntro Term Prop -- existsI
  | ExistsElim Term Prop -- existsE
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
  show (Fix t) = "fix " ++ show t
  show (ForallIntro t p) = "forallI " ++ show t ++ " " ++ show p
  show (ForallElim t p) = "forallE " ++ show t ++ " " ++ show p
  show (ExistsIntro t p) = "existsI " ++ show t ++ " " ++ show p
  show (ExistsElim t p) = "existsE " ++ show t ++ " " ++ show p

data ProofState = ProofState
  { goal :: Prop,
    assumptions :: [Prop],
    subProofs :: [ProofState],
    tactics :: [Tactic],
    completed :: Bool,
    fixed :: [Term]
  }
  deriving (Eq)

printProofState :: Int -> ProofState -> String
printProofState _ (ProofState _ _ _ _ True _) = ""
printProofState level state =
  unlines $
    [ indent ++ "Goal: " ++ show (goal state),
      indent ++ "Assumptions: " ++ show (assumptions state),
      indent ++ "Tactics: " ++ show (tactics state),
      indent ++ "Fixed: " ++ show (fixed state)
    ]
      ++ map (printProofState (level + 1)) (subProofs state)
  where
    indent = replicate (level * 2) ' '

instance Show ProofState where
  show = printProofState 0
