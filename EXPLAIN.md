# EXPLAIN: Explicação linha-a-linha

Este arquivo explica, linha-a-linha, os quatro arquivos principais do repositório: `Lexer.hs`, `Parser.y`, `TypeChecker.hs` e `Interpreter.hs`. Cada trecho de código é reproduzido e seguido por uma explicação concisa das linhas imediatamente abaixo.

---

**File: Lexer.hs**

```haskell
module Lexer where
```
- **Explicação:** Declara o módulo `Lexer`. Torna as definições deste arquivo disponíveis quando `import Lexer` for usado.

```haskell
import Data.Char
import Data.List (intercalate)
```
- **Explicação:** Importa funções para manipulação de caracteres (`isSpace`, `isDigit`, `isAlpha`, etc.) e `intercalate` (usado em outras partes para juntar listas com separador).

```haskell
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
```
- **Explicação:** Define o tipo algebraico `Token` que representa todos os tokens reconhecidos pelo lexer. Cada construtor corresponde a um token diferente (números com valor, strings com conteúdo, identificadores com nome, e tokens simbólicos/keywords sem payload). `deriving (Show, Eq)` permite imprimir e comparar tokens.

```haskell
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
          | Tuple [Expr] 
          | Proj Int Expr   
          | Let String Expr Expr    
          | Concat Expr Expr
          | Length Expr
          deriving Show
```
- **Explicação:** Define a AST (tipo `Expr`) que representa expressões da linguagem: números, booleanos, strings, operações aritméticas e lógicas, lambdas, aplicações, tuplas, projeção, `let`, concatenação e `length` de strings.

```haskell
data Ty = TNum 
        | TBool 
        | TString
        | TFun Ty Ty
        | TTuple [Ty] 
        deriving (Show, Eq) 
```
- **Explicação:** Tipo para as anotações de tipo usadas no typechecker e na AST: inteiro, booleano, string, funções (TFun) e tuplas (TTuple).

```haskell
lexer :: String -> [Token]
lexer [] = []
```
- **Explicação:** Assinatura e caso base da função `lexer`: converte uma `String` (entrada) em uma lista de `Token`. Se a entrada estiver vazia, retorna lista vazia.

```haskell
lexer ('\\':cs) = TokenLambda : lexer cs
lexer ('-':'>':cs) = TokenFun : lexer cs
lexer ('+':'+':cs) = TokenConcat : lexer cs
lexer ('+':cs) = TokenPlus : lexer cs 
lexer ('-':cs) = TokenSub : lexer cs
lexer ('*':cs) = TokenTimes : lexer cs 
lexer ('(':cs) = TokenLParen : lexer cs 
lexer (')':cs) = TokenRParen : lexer cs
lexer ('&':'&':cs) = TokenAnd : lexer cs 
lexer ('|':'|':cs) = TokenOr : lexer cs
lexer ('i':'f':cs) = TokenIf : lexer cs
lexer ('t':'h':'e':'n':cs) = TokenThen : lexer cs
lexer ('e':'l':'s':'e':cs) = TokenElse : lexer cs
lexer ('>':cs) = TokenGt : lexer cs
lexer ('=':'=':cs) = TokenEq : lexer cs
lexer ('<':cs) = TokenLt : lexer cs
lexer (':':cs) = TokenColon : lexer cs
lexer (',':cs) = TokenComma : lexer cs
lexer ('=':cs) = TokenEquals : lexer cs
```
- **Explicação:** Casos do analisador por padrão de prefixo: se a entrada começa com certos caracteres ou sequências, produz o token correspondente e continua com o restante (`cs`). A ordem importa — por exemplo `'=':'='` (==) é testado antes de `'='` simples.

```haskell
lexer ('\"':cs) =
  let (str, rest) = break (\c -> c == '\"' || c == '\n') cs
  in case rest of
       '\"':r -> TokenString str : lexer r
       '\n':_ -> error "Unclosed String"   
       _      -> error "Unclosed String"   
```
- **Explicação:** Trata literais de string: detecta `\"` (aspas entre escapes dentro de literais no código Haskell), usa `break` para coletar até a próxima `\"` ou `\n`. Se encontra `\"`, retorna `TokenString` com o conteúdo (`str`); se encontra `\n` ou fim inesperado, lança erro `Unclosed String`.

```haskell
lexer (c:cs)
  | isSpace c = lexer cs 
  | isDigit c = lexNum (c:cs)
  | isAlpha c = lexVarOrKw (c:cs)
lexer _ = error "Lexical error"
```
- **Explicação:** Caso genérico: ignora espaços (`isSpace`), números começam `lexNum`, letras começam `lexVarOrKw` (identificadores/keywords). Qualquer outro caso é erro léxico.

```haskell
lexNum :: String -> [Token]
lexNum cs = case span isDigit cs of 
  (num, rest) -> TokenNum (read num) : lexer rest 
```
- **Explicação:** `lexNum` agrupa dígitos contíguos com `span isDigit`, converte a string de dígitos em `Int` com `read` e adiciona `TokenNum` à lista; continua lexando o `rest`.

```haskell
lexVarOrKw :: String -> [Token]
lexVarOrKw cs = case span (\c -> isAlpha c || isDigit c) cs of 
  ("true", rest)  -> TokenTrue : lexer rest 
  ("false", rest) -> TokenFalse : lexer rest 
  ("if", rest)    -> TokenIf : lexer rest 
  ("then", rest)  -> TokenThen : lexer rest 
  ("else", rest)  -> TokenElse : lexer rest 
  ("Int", rest)   -> TokenTNum : lexer rest 
  ("Bool", rest)  -> TokenTBool : lexer rest  
  ("String", rest) -> TokenTString : lexer rest  
  ("proj", rest)  -> TokenProj : lexer rest 
  ("let", rest)   -> TokenLet : lexer rest 
  ("in", rest)    -> TokenIn : lexer rest  
  ("length", rest) -> TokenLength : lexer rest  
  (ident, rest)   -> 
    if null ident 
      then error "Empty Identificator"
      else TokenVarId ident : lexer rest 
```
- **Explicação:** Lê um identificador/keyword (letras ou dígitos). Se o lexema corresponde a uma keyword conhecida, retorna o token de keyword; caso contrário, produz `TokenVarId ident` para um identificador. Se `span` por algum motivo retornou `""`, lança erro.

---

**File: Parser.y**

```happy
{
module Parser where
import Lexer
}
```
- **Explicação:** Código Haskell embutido no topo do arquivo `.y` de Happy. O bloco entre `{` `}` é copiado para o parser gerado e importa o módulo `Lexer` (o tipo `Token` e AST).

```happy
%name parser Exp
%tokentype { Token }
%error { parseError }
```
- **Explicação:** Diretivas do Happy: o parser chamará `parser` e retorna `Exp`; o tipo dos tokens é `Token`; e `parseError` é a função de erro chamada em caso de falha sintática.

```happy
-- Precedência: aplicação é mais forte que operadores, mas Happy lida por regra de produção
%left '+' '-'
%left '*'
%right '->'
```
- **Explicação:** Declaração de precedência/associatividade para operadores: `+` e `-` são left-associative e têm mesma precedência; `*` tem precedência definida (a ordem das linhas influencia); `->` (função tipo) é right-associative.

```happy
%token
  num          { TokenNum $$ }
  true         { TokenTrue }
  false        { TokenFalse }
  string       { TokenString $$}
  "++"         { TokenConcat }
  "length"     { TokenLength }
  "String"     { TokenTString }
  '+'          { TokenPlus }
  '-'          { TokenSub }
  '*'          { TokenTimes }
  "&&"         { TokenAnd }
  "||"         { TokenOr }
  '('          { TokenLParen }
  ')'          { TokenRParen }
  "if"         { TokenIf }
  "then"       { TokenThen }
  "else"       { TokenElse }
  '>'          { TokenGt }
  "=="         { TokenEq }
  '<'          { TokenLt }
  '\\'         { TokenLambda }
  "->"         { TokenFun }
  ':'          { TokenColon }
  "Int"        { TokenTNum }  
  "Bool"       { TokenTBool } 
  ','          { TokenComma }
  "proj"       { TokenProj }  
  "let"        { TokenLet }
  "in"         { TokenIn } 
  '='          { TokenEquals } 
  varid        { TokenVarId $$ }
```
- **Explicação:** Declara os tokens reconhecidos pelo parser Happy e como construir cada `Token` a partir de elementos de entrada: `num` e `string` usam o valor `$$` passado pelo lexer; `varid` também passa o string do identificador.

```happy
%%

Exp : num                    { Num $1 }
    | true                   { BTrue }
    | false                  { BFalse }
    | string                 { Str $1 }
    | varid                  { Var $1 }
    | "let" varid '=' Exp "in" Exp  { Let $2 $4 $6 }
    | Exp "++" Exp           { Concat $1 $3}
    | "length" Exp           { Length $2 }
    | Exp '+' Exp            { Add $1 $3 }
    | Exp '-' Exp            { Sub $1 $3 }
    | Exp '*' Exp            { Times $1 $3 }
    | Exp '>' Exp            { Gt $1 $3 }
    | Exp "==" Exp           { Eq $1 $3 }
    | Exp '<' Exp            { Lt $1 $3 }
    | Exp "&&" Exp           { And $1 $3 }
    | Exp "||" Exp           { Or $1 $3 }
    | '(' Exp ',' ExpList ')'  { Tuple ($2 : $4) }
    | '(' Exp ')'            { $2 }
    | "if" Exp "then" Exp "else" Exp { If $2 $4 $6 }
    | '\\' varid ':' Type  "->" Exp    { Lam $2 $4 $6 }
    | Exp Exp                { App $1 $2 }
    | "proj" num Exp           { Proj $2 $3 }
```
- **Explicação (linha por linha da produção `Exp`):**
  - `num { Num $1 }`: quando o token `num` aparece, constrói `Num` com o valor do token (`$1`).
  - `true { BTrue }`: token `true` vira `BTrue` (nó booleano verdadeiro).
  - `false { BFalse }`: token `false` vira `BFalse`.
  - `string { Str $1 }`: `string` vira `Str` com conteúdo `$1`.
  - `varid { Var $1 }`: identificador vira `Var` com nome `$1`.
  - `"let" varid '=' Exp "in" Exp { Let $2 $4 $6 }`: produção `let x = e1 in e2` produz `Let x e1 e2` — `$2` é o `varid`, `$4` é o primeiro `Exp`, `$6` o segundo.
  - `Exp "++" Exp { Concat $1 $3 }`: concatenação de strings vira `Concat` com ambas as expressões.
  - `"length" Exp { Length $2 }`: aplica `length` a uma expressão de string.
  - `Exp '+' Exp { Add $1 $3 }`: soma, constrói `Add`.
  - `Exp '-' Exp { Sub $1 $3 }`: subtração, `Sub`.
  - `Exp '*' Exp { Times $1 $3 }`: multiplicação, `Times`.
  - `Exp '>' Exp { Gt $1 $3 }`: maior que, `Gt`.
  - `Exp "==" Exp { Eq $1 $3 }`: igualdade, `Eq`.
  - `Exp '<' Exp { Lt $1 $3 }`: menor que, `Lt`.
  - `Exp "&&" Exp { And $1 $3 }`: conjunção, `And`.
  - `Exp "||" Exp { Or $1 $3 }`: disjunção, `Or`.
  - `'(' Exp ',' ExpList ')' { Tuple ($2 : $4) }`: tupla com pelo menos dois elementos — constrói `Tuple` com primeiro `$2` e mais elementos `$4`.
  - `'(' Exp ')' { $2 }`: parênteses que apenas agrupam, devolve o subexpressão.
  - `"if" Exp "then" Exp "else" Exp { If $2 $4 $6 }`: expressão condicional.
  - `'\\' varid ':' Type  "->" Exp { Lam $2 $4 $6 }`: lambda anotado com tipo: `\x : T -> body` vira `Lam x T body`.
  - `Exp Exp { App $1 $2 }`: aplicação (aplica a expressão à esquerda à expressão à direita) produz `App`.
  - `"proj" num Exp { Proj $2 $3 }`: projeção de tupla: `proj n e` vira `Proj n e`.

```happy
Type : "Int"                 { TNum }
     | "Bool"                { TBool }
     | "String"              { TString }
     | '(' TypeList ')'      { TTuple $2 }
     | Type "->" Type        { TFun $1 $3 }
     | '(' Type ')'          { $2 }
```
- **Explicação (linha por linha de `Type`):**
  - `"Int" { TNum }`: token `Int` representa tipo numérico.
  - `"Bool" { TBool }`: token `Bool` representa tipo booleano.
  - `"String" { TString }`: token `String` representa tipo string.
  - `'(' TypeList ')' { TTuple $2 }`: par de parênteses com lista de tipos vira `TTuple` com a lista `$2`.
  - `Type "->" Type { TFun $1 $3 }`: tipo de função `A -> B` vira `TFun A B`.
  - `'(' Type ')' { $2 }`: parênteses em tipos apenas retraem.

```happy
ExpList : Exp                  { [$1] }
        | Exp ',' ExpList      { $1 : $3 }

TypeList : Type                 { [$1] }
         | Type ',' TypeList    { $1 : $3 }
```
- **Explicação:** Regras auxiliares para listas: `ExpList` e `TypeList` permitem escrever listas separadas por vírgulas; ação constrói listas Haskell.

```happy
{
parseError :: [Token] -> a 
parseError _ = error "Syntax error!"
}
```
- **Explicação:** Função de erro usada pelo parser: em caso de erro sintático, lança `error "Syntax error!"`.

---

**File: TypeChecker.hs**

```haskell
module TypeChecker where

import Lexer

-- Ambiente: lista de (variável, tipo)
type Ctx = [(String, Ty)]
```
- **Explicação:** Define o módulo `TypeChecker`, importa `Lexer` (para `Expr` e `Ty`) e define `Ctx` como contexto de tipos — uma lista de associações `nome -> tipo`.

```haskell
-- Tipagem com contexto
typeof :: Ctx -> Expr -> Maybe Ty
```
- **Explicação:** Assinatura de `typeof`: recebe contexto e expressão e retorna `Maybe Ty` (Just tipo se bem tipada, Nothing se não).

```haskell
typeof ctx (Num _) = Just TNum
typeof ctx BTrue = Just TBool
typeof ctx BFalse = Just TBool
typeof ctx (Str _) = Just TString
typeof ctx (Var x) = lookup x ctx
```
- **Explicação:** Casos base: números são `TNum`, booleans `TBool`, strings `TString`; variáveis consultam o contexto com `lookup`.

```haskell
typeof ctx (Lam x tp b) = let ctx' = (x, tp): ctx
                              in case typeof ctx' b of
                                Just tr -> Just (TFun tp tr)
                                _ -> Nothing
```
- **Explicação:** Lambda anotado: adiciona `(x,tp)` ao contexto e checa o corpo; se válido com tipo `tr`, o tipo da lambda é `TFun tp tr`.

```haskell
typeof ctx (App e1 e2) = case typeof ctx e1 of
                          Just (TFun tp tr) -> case typeof ctx e2 of
                                      Just t2 | t2 == tp -> Just tr
                                      _ -> Nothing
                          _ -> Nothing
```
- **Explicação:** Aplicação: primeiro verifica que `e1` tem tipo função `TFun tp tr`; então checa `e2` e exige que seu tipo seja `tp`. Se tudo OK, resultado é `tr`.

```haskell
typeof ctx (Add e1 e2) = case (typeof ctx e1, typeof ctx e2) of
  (Just TNum, Just TNum) -> Just TNum
  _ -> Nothing

... (Sub, Times analógico) ...
```
- **Explicação:** `Add`, `Sub`, `Times` exigem ambos operandos `TNum` e resultam em `TNum`. Caso contrário, `Nothing`.

```haskell
typeof ctx (And e1 e2) = case (typeof ctx e1, typeof ctx e2) of
  (Just TBool, Just TBool) -> Just TBool
  _ -> Nothing

typeof ctx (Or e1 e2) = case (typeof ctx e1, typeof ctx e2) of
  (Just TBool, Just TBool) -> Just TBool
  _ -> Nothing
```
- **Explicação:** `And` e `Or` exigem operandos booleanos e devolvem `TBool`.

```haskell
typeof ctx (If e1 e2 e3) = case typeof ctx e1 of
  Just TBool -> case (typeof ctx e2, typeof ctx e3) of
    (Just t2, Just t3) | t2 == t3 -> Just t2
                       | otherwise -> Nothing
    _ -> Nothing
  _ -> Nothing
```
- **Explicação:** `If` exige cond `TBool` e que os tipos das duas ramificações sejam iguais; retorna esse tipo.

```haskell
typeof ctx (Gt e1 e2) = 
  case (typeof ctx e1, typeof ctx e2) of
    (Just TNum, Just TNum) -> Just TBool
    _ -> Nothing
```
- **Explicação:** `Gt` (>) exige operandos numéricos e resulta em `TBool`.

```haskell
typeof ctx (Eq e1 e2) = 
  case (typeof ctx e1, typeof ctx e2) of
    (Just TNum, Just TNum) -> Just TBool
    (Just TBool, Just TBool) -> Just TBool
    (Just TString, Just TString) -> Just TBool 
    _ -> Nothing
```
- **Explicação:** `Eq` permite igualdade entre dois números, dois booleans ou duas strings; retorna `TBool` nesses casos.

```haskell
typeof ctx (Lt e1 e2) = 
  case (typeof ctx e1, typeof ctx e2) of
    (Just TNum, Just TNum) -> Just TBool
    _ -> Nothing
```
- **Explicação:** `Lt` (<) exige números e retorna `TBool`.

```haskell
typeof ctx (Tuple es) = do
  ts <- mapM (typeof ctx) es
  return (TTuple ts)
```
- **Explicação:** Para tuplas, tenta inferir tipo de cada elemento com `mapM` (falha se algum `Nothing`), e constrói `TTuple` com a lista de tipos.

```haskell
typeof ctx (Proj i e) = do
  te <- typeof ctx e
  case te of
    TTuple ts -> 
      if i >= 0 && i < length ts
        then Just (ts !! i)
        else error "Invalid Index"
    _ -> Nothing
```
- **Explicação:** Projeção: obtém tipo da expressão `e`, espera `TTuple ts`; se índice válido, retorna o tipo correspondente, caso contrário lança erro de índice inválido; se `e` não for tupla, retorna `Nothing`.

```haskell
typeof ctx (Let x e1 e2) = 
  case typeof ctx e1 of
    Just t1 -> typeof ((x, t1) : ctx) e2
    _ -> Nothing
```
- **Explicação:** `Let`: inferir o tipo de `e1`, estender contexto com `(x,t1)` e inferir `e2` nesse contexto.

```haskell
typeof ctx (Concat e1 e2) = 
  case (typeof ctx e1, typeof ctx e2) of
    (Just TString, Just TString) -> Just TString
    _ -> Nothing

typeof ctx (Length e) = 
  case typeof ctx e of
    Just TString -> Just TNum
    _ -> Nothing
```
- **Explicação:** `Concat` exige duas strings e retorna `TString`. `Length` exige string e retorna `TNum`.

```haskell
typecheck :: Expr -> Expr 
typecheck e = case typeof [] e of 
                Just _ -> e 
                _      -> error "Type error!"
```
- **Explicação:** Função de conveniência: checa se, no contexto vazio, a expressão tem tipo; se sim, devolve a própria expressão; caso contrário, lança `Type error!`.

---

**File: Interpreter.hs**

```haskell
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use record patterns" #-}
module Interpreter where

import Data.List (findIndex, intercalate)
import Lexer
```
- **Explicação:** Pragmas para GHC/HLint; declara módulo `Interpreter`, importa `findIndex` e `intercalate`, e importa `Lexer` (AST e tipos).

```haskell
isValue :: Expr -> Bool
isValue (Num _) = True
isValue BTrue = True
isValue BFalse = True
isValue (Str _) = True
isValue (Lam _ _ _) = True
isValue (Tuple es) = all isValue es
isValue (Proj _ _) = False
isValue _ = False
```
- **Explicação:** `isValue` diz se uma expressão é um valor final (não reduzível): números, booleans, strings, lambdas e tuplas de valores são valores; `Proj` nunca é valor por si só (precisa reduzir a tupla primeiro). Outros casos são `False`.

```haskell
subst :: String -> Expr -> Expr -> Expr
subst x s (Var v) = if x == v then s else Var v
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
```
- **Explicação:** Substituição capture-unsafe simples (não faz renomeação de variáveis): substitui todas ocorrências livres de `x` por `s` dentro da expressão de entrada. Na definição de `Lam`, se o parâmetro `v` é igual a `x`, não substitui dentro do corpo (protege a variável ligada). Em `Let`, substitui no `e1`, e em `e2` só se o nome for diferente.

```haskell
step :: Expr -> Expr
step (Add (Num n1) (Num n2)) = Num (n1 + n2)
step (Add (Num n1) e2) = Add (Num n1) (step e2)
step (Add e1 e2) = Add (step e1) e2
```
- **Explicação:** Regras de redução para `Add` (avaliação left-to-right): se ambos são números, calcula; se o primeiro operando é número e o segundo não é valor, reduz o segundo; caso contrário reduz o primeiro.

```haskell
step (Sub (Num n1) (Num n2)) = Num (n1 - n2)
step (Sub (Num n1) e2) = Sub (Num n1) (step e2)
step (Sub e1 e2) = Sub (step e1) e2
step (Times (Num n1) (Num n2)) = Num (n1 * n2)
step (Times (Num n1) e2) = Times (Num n1) (step e2)
step (Times e1 e2) = Times (step e1) e2
```
- **Explicação:** Mesma ideia para `Sub` e `Times`.

```haskell
step (And BTrue e2) = e2
step (And BFalse e2) = BFalse
step (And e1 e2) = And (step e1) e2
step (Or BTrue e2) = BTrue
step (Or BFalse e2) = e2
step (Or e1 e2) = Or (step e1) e2
```
- **Explicação:** Avaliação curta-circuito para operadores lógicos: `And` e `Or` seguem regras padrão com avaliação left-to-right.

```haskell
step (If BTrue e1 e2) = e1
step (If BFalse e1 e2) = e2
step (If e1 e2 e3) = If (step e1) e2 e3
```
- **Explicação:** `If` reduz a condição primeiro e escolhe a ramificação certa quando a condição é um booleano.

```haskell
step (Gt (Num n1) (Num n2)) = if n1 > n2 then BTrue else BFalse
step (Gt (Num n1) e2) = Gt (Num n1) (step e2)
step (Gt e1 e2) = Gt (step e1) e2
```
- **Explicação:** Comparação `Gt` similar a aritmética: reduz operandos e calcula quando ambos são números.

```haskell
step (Eq (Num n1) (Num n2)) = if n1 == n2 then BTrue else BFalse
step (Eq BTrue BTrue) = BTrue
step (Eq BFalse BFalse) = BTrue
step (Eq BTrue BFalse) = BFalse
step (Eq BFalse BTrue) = BFalse
step (Eq (Str s1) (Str s2)) = if s1 == s2 then BTrue else BFalse
step (Eq (Num n1) e2) = Eq (Num n1) (step e2)
step (Eq BTrue e2) = Eq BTrue (step e2)
step (Eq BFalse e2) = Eq BFalse (step e2)
step (Eq (Str s1) e2) = Eq (Str s1) (step e2)
step (Eq e1 e2) = Eq (step e1) e2
```
- **Explicação:** Igualdade implementada ponto a ponto para números, booleans e strings; regras para redução dos operandos até que possam ser comparados.

```haskell
step (Lt (Num n1) (Num n2)) = if n1 < n2 then BTrue else BFalse
step (Lt (Num n1) e2) = Lt (Num n1) (step e2)
step (Lt e1 e2) = Lt (step e1) e2
```
- **Explicação:** Igual que `Gt`, mas para `<`.

```haskell
step (App (Lam x tp e1) e2) =
  if isValue e2
    then subst x e2 e1
    else App (Lam x tp e1) (step e2)
step (App e1 e2) =
  if isValue e1
    then App e1 (step e2)
    else App (step e1) e2
```
- **Explicação:** Aplicação: se a função é um `Lam` e o argumento é valor, faz substituição (beta-reduction); caso contrário, reduz argumento ou função conforme necessário (ordem de avaliação esquerda para direita).

```haskell
step (Tuple es) =
  case findIndex (not . isValue) es of
    Nothing -> Tuple es
    Just i ->
      let (before, e : after) = splitAt i es
          e' = step e
       in Tuple (before ++ e' : after)
```
- **Explicação:** Para tuplas, encontra o primeiro elemento que não é valor e o reduz; se todos forem valores, a tupla é considerada valor.

```haskell
step (Proj i (Tuple es)) =
  if i >= 0 && i < length es
    then es !! i
    else error "Invalid Index"
step (Proj i e) = Proj i (step e)
```
- **Explicação:** Projeção: se a expressão é uma tupla de valores, retorna o elemento no índice; se a tupla ainda precisa ser avaliada, reduz `e`.

```haskell
step (Concat (Str s1) (Str s2)) = Str (s1 ++ s2)
step (Concat (Str s1) e2) = Concat (Str s1) (step e2)
step (Concat e1 e2) = Concat (step e1) e2
```
- **Explicação:** Concatenação de strings: concatena quando ambos os operandos são strings; caso contrário, reduz operandos.

```haskell
step (Length (Str s)) = Num (length s)
step (Length e) = Length (step e)
```
- **Explicação:** `Length` calcula o tamanho de uma string quando o argumento é valor string; caso contrário, reduz o argumento.

```haskell
step (Let x e1 e2) =
  App (Lam x dummyType e2) e1
  where
    dummyType = TNum
```
- **Explicação:** Implementa `let x = e1 in e2` transformando em aplicação de lambda: `(\x : _ -> e2) e1`. Usa um `dummyType` porque o interpretador não precisa do tipo para avaliação — o typechecker separadamente valida tipos.

```haskell
step e = error $ "Error: " ++ show e
```
- **Explicação:** Caso padrão: se nenhuma regra de `step` se aplica, gera erro contendo a expressão (indica redução inválida ou termo irreduzível inesperado).

```haskell
eval :: Expr -> Expr
eval (Let x e1 e2) =
  let v1 = eval e1
      e2' = subst x v1 e2
   in eval e2'
eval e = if isValue e then e else eval (step e)
```
- **Explicação:** `eval` realiza avaliação completa: em `Let` avalia `e1`, substitui e avalia `e2'`. Caso geral: se `e` é valor, retorna-o; senão, aplica `step` recursivamente até chegar a valor.

```haskell
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
```
- **Explicação:** Imprime uma representação legível da expressão final (ou intermediária). `printPretty` converte a `Expr` em `String` mostrando operadores infix legíveis e estruturas como `if`, `let`, `proj`, etc. Observação: para `Lam` o tipo não é impresso corretamente (há um espaço faltando para o tipo — mas isso é estética).

---

Se quiser, eu:
- posso dividir ainda mais, adicionando números de linha e referências precisas (ex.: `Lexer.hs:23`) — responda "numerar";
- posso gerar um `Lexer.x` equivalente para Alex a partir do `Lexer.hs` — responda "alex";
- posso extrair trechos do `Parser.hs` gerado e mostrar exatamente como cada produção do `Parser.y` vira uma `happyReduce_*` — responda "mapear".

Tarefa marcada como concluída no TODO localmente.
