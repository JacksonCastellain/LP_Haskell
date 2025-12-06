module TypeChecker where

import Lexer

-- Ambiente: lista de (variável, tipo)
type Ctx = [(String, Ty)]

-- Tipagem com contexto
typeof :: Ctx -> Expr -> Maybe Ty

typeof ctx (Num _) = Just TNum
typeof ctx BTrue = Just TBool
typeof ctx BFalse = Just TBool
typeof ctx (Str _) = Just TString
typeof ctx (Var x) = lookup x ctx
typeof ctx (Lam x tp b) = let ctx' = (x, tp): ctx
    in case typeof ctx' b of
      Just tr -> Just (TFun tp tr)
      _ -> Nothing
typeof ctx (App e1 e2) = case typeof ctx e1 of
    Just (TFun tp tr) -> case typeof ctx e2 of
                Just t2 | t2 == tp -> Just tr
                _ -> Nothing
    _ -> Nothing

typeof ctx (Add e1 e2) = case (typeof ctx e1, typeof ctx e2) of
  (Just TNum, Just TNum) -> Just TNum
  _ -> Nothing

typeof ctx (Sub e1 e2) = case (typeof ctx e1, typeof ctx e2) of
  (Just TNum, Just TNum) -> Just TNum
  _ -> Nothing

typeof ctx (Times e1 e2) = case (typeof ctx e1, typeof ctx e2) of
  (Just TNum, Just TNum) -> Just TNum
  _ -> Nothing

typeof ctx (And e1 e2) = case (typeof ctx e1, typeof ctx e2) of
  (Just TBool, Just TBool) -> Just TBool
  _ -> Nothing

typeof ctx (Or e1 e2) = case (typeof ctx e1, typeof ctx e2) of
  (Just TBool, Just TBool) -> Just TBool
  _ -> Nothing

typeof ctx (If e1 e2 e3) = case typeof ctx e1 of
  Just TBool -> case (typeof ctx e2, typeof ctx e3) of
    (Just t2, Just t3) | t2 == t3 -> Just t2
                       | otherwise -> Nothing
    _ -> Nothing
  _ -> Nothing

typeof ctx (Gt e1 e2) = 
  case (typeof ctx e1, typeof ctx e2) of
    (Just TNum, Just TNum) -> Just TBool
    _ -> Nothing

typeof ctx (Eq e1 e2) = 
  case (typeof ctx e1, typeof ctx e2) of
    (Just TNum, Just TNum) -> Just TBool
    (Just TBool, Just TBool) -> Just TBool
    (Just TString, Just TString) -> Just TBool 
    _ -> Nothing

typeof ctx (Lt e1 e2) = 
  case (typeof ctx e1, typeof ctx e2) of
    (Just TNum, Just TNum) -> Just TBool
    _ -> Nothing

typeof ctx (Tuple es) = do
  ts <- mapM (typeof ctx) es
  return (TTuple ts)

typeof ctx (Proj i e) = do
  te <- typeof ctx e
  case te of
    TTuple ts -> 
      if i >= 0 && i < length ts
        then Just (ts !! i)
        else error "Invalid Index"
    _ -> Nothing
    
typeof ctx (Let x e1 e2) = 
  case typeof ctx e1 of
    Just t1 -> typeof ((x, t1) : ctx) e2
    _ -> Nothing

typeof ctx (Concat e1 e2) = 
  case (typeof ctx e1, typeof ctx e2) of
    (Just TString, Just TString) -> Just TString
    _ -> Nothing

typeof ctx (Length e) = 
  case typeof ctx e of
    Just TString -> Just TNum
    _ -> Nothing

typecheck :: Expr -> Expr 
typecheck e = case typeof [] e of 
                Just _ -> e 
                _      -> error "Type error!"

