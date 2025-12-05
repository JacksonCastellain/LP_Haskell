{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use record patterns" #-}
module Interpreter where

import Data.List (findIndex, intercalate)
import Lexer

isValue :: Expr -> Bool
isValue (Num _) = True
isValue BTrue = True
isValue BFalse = True
isValue (Str _) = True
isValue (Lam _ _ _) = True
isValue (Tuple es) = all isValue es
isValue (Proj _ _) = False
isValue _ = False

subst :: String -> Expr -> Expr -> Expr
subst x s (Var v) = 
  if x == v 
    then s 
    else Var v
subst x s (Lam v tp b) =
  if x == v
    then Lam v tp b
    else Lam v tp (subst x s b)
subst x s (Str str) = Str str
subst x s (App t1 t2) = App (subst x s t1) (subst x s t2)
subst x s (Add t1 t2) = Add (subst x s t1) (subst x s t2)
subst x s (Sub t1 t2) = Sub (subst x s t1) (subst x s t2)
subst x s (Times t1 t2) = Times (subst x s t1) (subst x s t2)
subst x s (And t1 t2) = And (subst x s t1) (subst x s t2)
subst x s (Or t1 t2) = Or (subst x s t1) (subst x s t2)
subst x s (If t1 t2 t3) = If (subst x s t1) (subst x s t2) (subst x s t3)
subst x s (Gt e1 e2) = Gt (subst x s e1) (subst x s e2)
subst x s (Eq e1 e2) = Eq (subst x s e1) (subst x s e2)
subst x s (Lt e1 e2) = Lt (subst x s e1) (subst x s e2)
subst x s (Tuple es) = Tuple (map (subst x s) es)
subst x s (Proj i e) = Proj i (subst x s e)
subst x s (Concat e1 e2) = Concat (subst x s e1) (subst x s e2)
subst x s (Length e) = Length (subst x s e)
subst x s (Let y e1 e2) =
  if x == y
    then Let y (subst x s e1) e2
    else Let y (subst x s e1) (subst x s e2)
subst x s e = e

step :: Expr -> Expr
step (Add (Num n1) (Num n2)) = Num (n1 + n2)
step (Add (Num n1) e2) = Add (Num n1) (step e2)
step (Add e1 e2) = Add (step e1) e2
step (Sub (Num n1) (Num n2)) = Num (n1 - n2)
step (Sub (Num n1) e2) = Sub (Num n1) (step e2)
step (Sub e1 e2) = Sub (step e1) e2
step (Times (Num n1) (Num n2)) = Num (n1 * n2)
step (Times (Num n1) e2) = Times (Num n1) (step e2)
step (Times e1 e2) = Times (step e1) e2
step (And BTrue e2) = e2
step (And BFalse e2) = BFalse
step (And e1 e2) = And (step e1) e2
step (Or BTrue e2) = BTrue
step (Or BFalse e2) = e2
step (Or e1 e2) = Or (step e1) e2
step (If BTrue e1 e2) = e1
step (If BFalse e1 e2) = e2
step (If e1 e2 e3) = If (step e1) e2 e3
step (Gt (Num n1) (Num n2)) = 
  if n1 > n2 
    then BTrue 
    else BFalse
step (Gt (Num n1) e2) = Gt (Num n1) (step e2)
step (Gt e1 e2) = Gt (step e1) e2
step (Eq (Num n1) (Num n2)) = 
  if n1 == n2 
    then BTrue 
    else BFalse
step (Eq BTrue BTrue) = BTrue
step (Eq BFalse BFalse) = BFalse
step (Eq BTrue BFalse) = BFalse
step (Eq BFalse BTrue) = BFalse
step (Eq (Str s1) (Str s2)) = 
  if s1 == s2 
    then BTrue 
    else BFalse
step (Eq (Num n1) e2) = Eq (Num n1) (step e2)
step (Eq BTrue e2) = Eq BTrue (step e2)
step (Eq BFalse e2) = Eq BFalse (step e2)
step (Eq (Str s1) e2) = Eq (Str s1) (step e2)
step (Eq e1 e2) = Eq (step e1) e2
step (Lt (Num n1) (Num n2)) = 
  if n1 < n2 
    then BTrue 
    else BFalse
step (Lt (Num n1) e2) = Lt (Num n1) (step e2)
step (Lt e1 e2) = Lt (step e1) e2
step (App (Lam x tp e1) e2) =
  if isValue e2
    then subst x e2 e1
    else App (Lam x tp e1) (step e2)
step (App e1 e2) =
  if isValue e1
    then App e1 (step e2)
    else App (step e1) e2
step (Tuple es) =
  case findIndex (not . isValue) es of
    Nothing -> Tuple es
    Just i ->
      let (before, e : after) = splitAt i es
          e' = step e
       in Tuple (before ++ e' : after)
step (Proj i (Tuple es)) =
  if i >= 0 && i < length es
    then es !! i
    else error "Invalid Index"
step (Proj i e) = Proj i (step e)
step (Concat (Str s1) (Str s2)) = Str (s1 ++ s2)
step (Concat (Str s1) e2) = Concat (Str s1) (step e2)
step (Concat e1 e2) = Concat (step e1) e2
step (Length (Str s)) = Num (length s)
step (Length e) = Length (step e)
step (Let x e1 e2) =
  if isValue e1 
    then subst x e1 e2 
    else Let x (step e1) e2
step e = error $ "Error: " ++ show e

eval :: Expr -> Expr
eval (Let x e1 e2) =
  let v1 = eval e1
      e2' = subst x v1 e2
   in eval e2'
eval e = if isValue e 
          then e 
          else eval (step e)

printPretty :: Expr -> String
printPretty (Num n) = show n
printPretty BTrue = "true"
printPretty BFalse = "false"
printPretty (Str s) = show s
printPretty (Var x) = x
printPretty (Add e1 e2) = "(" ++ printPretty e1 ++ " + " ++ printPretty e2 ++ ")"
printPretty (Sub e1 e2) = "(" ++ printPretty e1 ++ " - " ++ printPretty e2 ++ ")"
printPretty (Times e1 e2) = "(" ++ printPretty e1 ++ " * " ++ printPretty e2 ++ ")"
printPretty (Gt e1 e2) = "(" ++ printPretty e1 ++ " > " ++ printPretty e2 ++ ")"
printPretty (Eq e1 e2) = "(" ++ printPretty e1 ++ " == " ++ printPretty e2 ++ ")"
printPretty (Lt e1 e2) = "(" ++ printPretty e1 ++ " < " ++ printPretty e2 ++ ")"
printPretty (And e1 e2) = "(" ++ printPretty e1 ++ " && " ++ printPretty e2 ++ ")"
printPretty (Or e1 e2) = "(" ++ printPretty e1 ++ " || " ++ printPretty e2 ++ ")"
printPretty (Concat e1 e2) = "(" ++ printPretty e1 ++ " ++ " ++ printPretty e2 ++ ")"
printPretty (Length e) = "length " ++ printPretty e
printPretty (If e1 e2 e3) = "(if " ++ printPretty e1 ++ " then " ++ printPretty e2 ++ " else " ++ printPretty e3 ++ ")"
printPretty (Lam x ty body) = "(\\" ++ x ++ " :  -> " ++ printPretty body ++ ")"
printPretty (App e1 e2) = "(" ++ printPretty e1 ++ " " ++ printPretty e2 ++ ")"
printPretty (Tuple es) = "(" ++ intercalate ", " (map printPretty es) ++ ")"
printPretty (Proj i e) = "proj " ++ show i ++ " " ++ printPretty e
printPretty (Let x e1 e2) = "(let " ++ x ++ " = " ++ printPretty e1 ++ " in " ++ printPretty e2 ++ ")"
