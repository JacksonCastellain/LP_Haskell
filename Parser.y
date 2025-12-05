{
module Parser where
import Lexer
}

%name parser Exp
%tokentype { Token }
%error { parseError }

-- Precedência
%left '+' '-'
%left '*'
%right '->'

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

%%

Exp : num                            { Num $1 }
    | true                           { BTrue }
    | false                          { BFalse }
    | string                         { Str $1 }
    | varid                          { Var $1 }
    | "let" varid '=' Exp "in" Exp   { Let $2 $4 $6 }
    | Exp "++" Exp                   { Concat $1 $3}
    | "length" Exp                   { Length $2 }
    | Exp '+' Exp                    { Add $1 $3 }
    | Exp '-' Exp                    { Sub $1 $3 }
    | Exp '*' Exp                    { Times $1 $3 }
    | Exp '>' Exp                    { Gt $1 $3 }
    | Exp "==" Exp                   { Eq $1 $3 }
    | Exp '<' Exp                    { Lt $1 $3 }
    | Exp "&&" Exp                   { And $1 $3 }
    | Exp "||" Exp                   { Or $1 $3 }
    | '(' Exp ',' ExpList ')'        { Tuple ($2 : $4) }
    | '(' Exp ')'                    { $2 }
    | "if" Exp "then" Exp "else" Exp { If $2 $4 $6 }
    | '\\' varid ':' Type  "->" Exp  { Lam $2 $4 $6 }
    | Exp Exp                        { App $1 $2 }
    | "proj" num Exp                 { Proj $2 $3 }
    
Type : "Int"                         { TNum }
     | "Bool"                        { TBool }
     | "String"                      { TString }
     | '(' TypeList ')'              { TTuple $2 }
     | Type "->" Type                { TFun $1 $3 }
     | '(' Type ')'                  { $2 }
    
ExpList : Exp                        { [$1] }
        | Exp ',' ExpList            { $1 : $3 }
    
TypeList : Type                      { [$1] }
         | Type ',' TypeList         { $1 : $3 }


{
parseError :: [Token] -> a 
parseError _ = error "Syntax error!"
}