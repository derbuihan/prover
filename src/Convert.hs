module Convert where

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
