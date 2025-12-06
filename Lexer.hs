module Lexer where

import Data.Char
import Data.List (intercalate)

data Token = TokenNum Int 
           | TokenTrue 
           | TokenFalse
           | TokenPlus 
           | TokenSub
           | TokenTimes 
           | TokenAnd 
           | TokenOr 
           | TokenLParen 
           | TokenRParen 
           | TokenIf
           | TokenThen
           | TokenElse
           | TokenGt
           | TokenEq
           | TokenLt
           | TokenLambda
           | TokenFun
           | TokenColon
           | TokenTNum
           | TokenTBool
           | TokenTString
           | TokenString String
           | TokenVarId String
           | TokenComma
           | TokenProj
           | TokenLet 
           | TokenIn  
           | TokenEquals
           | TokenConcat
           | TokenLength 
           deriving (Show, Eq)

data Expr = Num Int 
          | BTrue 
          | BFalse 
          | Str String
          | Add Expr Expr 
          | Sub Expr Expr
          | Times Expr Expr 
          | And Expr Expr 
          | Or Expr Expr 
          | Paren Expr
          | If Expr Expr Expr
          | Gt Expr Expr
          | Eq Expr Expr
          | Lt Expr Expr
          | Var String
          | Lam String Ty Expr
          | App Expr Expr
          | Let String Expr Expr    
          | Concat Expr Expr
          | Length Expr
          | Proj Int Expr   
          | Tuple [Expr] 
          deriving Show


data Ty = TNum 
        | TBool 
        | TString
        | TFun Ty Ty
        | TTuple [Ty] 
        deriving (Show, Eq) 


lexer :: String -> [Token]
lexer [] = []
lexer ('+':cs) = TokenPlus : lexer cs 
lexer ('-':cs) = TokenSub : lexer cs
lexer ('*':cs) = TokenTimes : lexer cs 
lexer ('(':cs) = TokenLParen : lexer cs 
lexer (')':cs) = TokenRParen : lexer cs
lexer ('&':'&':cs) = TokenAnd : lexer cs 
lexer ('|':'|':cs) = TokenOr : lexer cs
lexer ('>':cs) = TokenGt : lexer cs
lexer ('=':'=':cs) = TokenEq : lexer cs
lexer ('<':cs) = TokenLt : lexer cs
lexer (':':cs) = TokenColon : lexer cs
lexer (',':cs) = TokenComma : lexer cs
lexer ('=':cs) = TokenEquals : lexer cs
lexer ('\\':cs) = TokenLambda : lexer cs
lexer ('-':'>':cs) = TokenFun : lexer cs
lexer ('+':'+':cs) = TokenConcat : lexer cs
lexer ('\"':cs) =
  let (str, rest) = break (\c -> c == '\"' || c == '\n') cs
  in case rest of
       '\"':r -> TokenString str : lexer r
       '\n':_ -> error "Unclosed String"   
       _      -> error "Unclosed String"   

lexer (c:cs)
  | isSpace c = lexer cs 
  | isDigit c = lexNum (c:cs)
  | isAlpha c = lexVarOrKw (c:cs)
lexer _ = error "Lexical error"

lexNum :: String -> [Token]
lexNum cs = case span isDigit cs of 
  (num, rest) -> TokenNum (read num) : lexer rest 

lexVarOrKw :: String -> [Token]
lexVarOrKw cs = case span (\c -> isAlpha c || isDigit c) cs of 
  ("true", rest)  -> TokenTrue : lexer rest 
  ("false", rest) -> TokenFalse : lexer rest 
    ("proj", rest)  -> TokenProj : lexer rest 
  ("let", rest)   -> TokenLet : lexer rest 
  ("in", rest)    -> TokenIn : lexer rest  
  ("length", rest) -> TokenLength : lexer rest  
  ("if", rest)    -> TokenIf : lexer rest 
  ("then", rest)  -> TokenThen : lexer rest 
  ("else", rest)  -> TokenElse : lexer rest 
  ("Int", rest)   -> TokenTNum : lexer rest 
  ("Bool", rest)  -> TokenTBool : lexer rest  
  ("String", rest) -> TokenTString : lexer rest  
  (ident, rest)   -> 
    if null ident 
      then error "Empty Identificator"
      else TokenVarId ident : lexer rest 