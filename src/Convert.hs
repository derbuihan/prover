module Convert where

import Parser
import Types

type Env = [(String, Int)]

shiftEnv :: String -> Env -> Env
shiftEnv s env = (s, 0) : [(v, i + 1) | (v, i) <- env]

-- de Bruijn Index

convertProp :: Prop -> Prop
convertProp = convertProp_ []

convertProp_ :: Env -> Prop -> Prop
convertProp_ _ Falsum = Falsum
convertProp_ env (Atom s ts) = Atom s (map (convertTerm env) ts)
convertProp_ env (Not p) = Not (convertProp_ env p)
convertProp_ env (And p1 p2) = And (convertProp_ env p1) (convertProp_ env p2)
convertProp_ env (Or p1 p2) = Or (convertProp_ env p1) (convertProp_ env p2)
convertProp_ env (Imp p1 p2) = Imp (convertProp_ env p1) (convertProp_ env p2)
convertProp_ env (Iff p1 p2) = Iff (convertProp_ env p1) (convertProp_ env p2)
convertProp_ env (Forall s p) =
  let env_ = shiftEnv s env
   in Forall s (convertProp_ env_ p)
convertProp_ env (Exists s p) =
  let env_ = shiftEnv s env
   in Exists s (convertProp_ env_ p)

convertTerm :: Env -> Term -> Term
convertTerm env (Var s) =
  case lookup s env of
    Just i -> VarInt i
    Nothing -> Var s
convertTerm env (Func s ts) = Func s (map (convertTerm env) ts)
convertTerm _ (VarInt i) = VarInt i

-- Parse and Convert

parseAndConvertProp :: String -> Prop
parseAndConvertProp s =
  let prop = parseProp s
   in convertProp prop

parseAndConvertAssumptions :: String -> [Prop]
parseAndConvertAssumptions = map convertProp . parseAssumptions

parseAndConvertTactic :: String -> Tactic
parseAndConvertTactic s =
  let tokens = tokenize s
      (tactic, rest) = parseTactic_ tokens
   in case rest of
        [TEOF] -> convertTactic tactic
        _ -> error "Parsing failed, unexpected tokens remaining"

convertTactic :: Tactic -> Tactic
convertTactic (Assume p q) =
  let p_ = convertProp p
      q_ = convertProp q
   in Assume p_ q_
convertTactic (Suppose p) =
  let p_ = convertProp p
   in Suppose p_
convertTactic (AndIntro p) =
  let p_ = convertProp p
   in AndIntro p_
convertTactic (AndElimLeft p) =
  let p_ = convertProp p
   in AndElimLeft p_
convertTactic (AndElimRight p) =
  let p_ = convertProp p
   in AndElimRight p_
convertTactic (OrIntro p) =
  let p_ = convertProp p
   in OrIntro p_
convertTactic (OrElim p q) =
  let p_ = convertProp p
      q_ = convertProp q
   in OrElim p_ q_
convertTactic (ImpIntro p) =
  let p_ = convertProp p
   in ImpIntro p_
convertTactic (ImpElim p1 p2) =
  let p1_ = convertProp p1
      p2_ = convertProp p2
   in ImpElim p1_ p2_
convertTactic (Dn p) =
  let p_ = convertProp p
   in Dn p_
convertTactic (Contra p1 p2) =
  let p1_ = convertProp p1
      p2_ = convertProp p2
   in Contra p1_ p2_
convertTactic Done = Done
convertTactic (Fix t) =
  let t_ = convertTerm [] t
   in Fix t_
convertTactic (ForallIntro t p) =
  let p_ = convertProp p
   in ForallIntro t p_
convertTactic (ForallElim t p) =
  let p_ = convertProp p
   in ForallElim t p_
convertTactic (ExistsIntro t p) =
  let p_ = convertProp p
   in ExistsIntro t p_
convertTactic (ExistsElim t p) =
  let p_ = convertProp p
   in ExistsElim t p_
