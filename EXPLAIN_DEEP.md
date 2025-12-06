# EXPLAIN_DEEP: Entendendo profundamente o projeto LP_Haskell

Este documento serve como um guia detalhado e pedagógico para entender o que cada peça do repositório faz, por que ela existe, como os dados fluem entre os componentes e como raciocinar passo a passo sobre programas de exemplo.

Sumário (o que você vai encontrar aqui)
- Visão conceitual: papel de lexer, parser, typechecker, interpreter
- Modelos de dados fundamentais: `Token`, `Expr`, `Ty` — campos, invariantes e exemplos
- Gramática (produções do `Parser.y`) explicada com exemplos concretos
- Walkthrough completo (duas entradas): tokens → AST → tipo → avaliação (pequeno e médio exemplo)
- Semântica operacional (pequeno-step / big-step intuitivo) e estratégia de avaliação
- Regras de tipagem (formato de inferência) para os construtos principais
- Como depurar cada etapa (receitas práticas e comandos)
- Erros comuns, limitações e como resolvê-los (ex.: captura de variável, escapes em strings)
- Extensões sugeridas com esboços de solução (escape de strings, alpha-renaming, Either errors)
- Comandos úteis e checklist para evoluir o projeto

---

1) Visão conceitual (por que cada componente existe)
- Lexer (`Lexer.hs`): converte caracteres em tokens. Raciocínio: separar o problema de parsing em duas camadas — reconhecer lexemas (números, strings, palavras-reservadas e símbolos) e, depois, analisar estrutura sintática (como expressões e listas). Isso simplifica o parser e facilita suporte a formatos (e.g., comentários, escapes) alterando apenas o lexer.

- Parser (`Parser.y` / `Parser.hs`): converte tokens em uma AST (`Expr`). Raciocínio: a gramática captura a estrutura da linguagem e ações semânticas (blocos entre `{ ... }`) constroem os nós da AST. Happy resolve conflitos LR, precedence e associaitividade para operadores.

- TypeChecker (`TypeChecker.hs`): garante que as operações são usadas com tipos compatíveis. Raciocínio: evita erros de runtime (ex.: somar booleanos) e documenta invariantes que o interpretador pode assumir (por exemplo, `Concat` só recebe `TString`).

- Interpreter (`Interpreter.hs`): define semântica operacional via `step` (um passo) e `eval` (redução completa). Raciocínio: ter `step` permite tanto depuração (inspecionar passos) quanto implementação clara de regras (beta-reduction, avaliação curta-circuito).

2) Modelos de dados — o que representam e invariantes
- Token (em `Lexer.hs`)
  - Cada construtor representa uma categoria léxica: `TokenNum Int`, `TokenString String`, tokens simbólicos (`TokenPlus`, `TokenEq`) e keywords (`TokenIf`, `TokenLet`).
  - Invariantes: `TokenNum n` garante que `n` é um inteiro válido; `TokenString s` não contém a aspa delimitadora (o lexer remove-as). O parser espera receber tokens coerentes com `Parser.y`.

- Expr (AST)
  - Ex.: `Num Int`, `Str String`, `Var String`, `Add Expr Expr`, `Lam String Ty Expr`, `App Expr Expr`, `Tuple [Expr]`, `Proj Int Expr`, `Let String Expr Expr`.
  - Invariantes: uma `Lam` sempre leva um `Ty` (anotação); `Tuple` lista zero ou mais `Expr` (a gramática cria tuplas com >= 2 elementos, mas o AST permite listas). Certifique-se que `Proj` seja aplicado a uma expressão que de fato tenha tipo `TTuple` (typechecker confere isso).

- Ty (tipos)
  - `TNum`, `TBool`, `TString`, `TFun Ty Ty`, `TTuple [Ty]`.
  - Invariantes: tipos são sintaticamente simples e a linguagem não faz inferência por padrão (funções precisam de anotações no `Parser.y`).

3) Gramática: produções principais explicadas com exemplos reais
(vez por vez: produção -> o que aceita -> exemplo textual -> AST resultante)

- `num { Num $1 }` — aceita tokens numéricos.
  - Exemplo: fonte `42` → token `TokenNum 42` → AST `Num 42`.

- `true` / `false` → `BTrue`, `BFalse`.
  - Ex.: `true` → `BTrue`.

- `string { Str $1 }` — literals de string com conteúdo.
  - Ex.: `"Olá"` → `TokenString "Olá"` → AST `Str "Olá"`.

- `varid { Var $1 }` — identifiers
  - Ex.: `x` → `Var "x"`.

- `"let" varid '=' Exp "in" Exp  { Let $2 $4 $6 }`
  - Aceita: `let x = e1 in e2`.
  - Ex.: `let a = 1 in a + 2` → AST `Let "a" (Num 1) (Add (Var "a") (Num 2))`.

- `Exp "++" Exp { Concat $1 $3}`
  - Ex.: `"a" ++ "b"` → `Concat (Str "a") (Str "b")`.

- `"length" Exp { Length $2 }` — ex.: `length "ab"` → `Length (Str "ab")`.

- `Exp '+' Exp { Add $1 $3 }` e análogos para `-`, `*` — operadores aritméticos.
  - Ex.: `1 + 2` → `Add (Num 1) (Num 2)`.

- `Exp '>' Exp { Gt $1 $3 }`, `Exp "==" Exp { Eq $1 $3 }`, `Exp '<' Exp { Lt $1 $3 }` — comparações.

- Tupla: `'(' Exp ',' ExpList ')' { Tuple ($2 : $4) }` — aceita `(e1, e2, ...)`.
  - Ex.: `(1, 2, 3)` → `Tuple [Num 1, Num 2, Num 3]`.

- `"if" Exp "then" Exp "else" Exp { If $2 $4 $6 }` — condicional.
  - Ex.: `if x > 0 then x else 0`.

- Lambda anotado: `'\\' varid ':' Type  "->" Exp { Lam $2 $4 $6 }` — ex.: `\x : Int -> x + 1` → `Lam "x" TNum (Add (Var "x") (Num 1))`.

- Aplicação: `Exp Exp { App $1 $2 }` — aplicação direta; note ambiguidade potencial com `Exp '+' Exp` etc.; precedence e regras no Happy, e a forma das produções, resolvem isso.

- `proj`: `"proj" num Exp { Proj $2 $3 }` — ex.: `proj 0 (10, 20)`.

4) Walkthrough completo — exemplo 1 (simples)
Fonte: (exemplo do README)

let nome = "Ana" in
let msg  = "Olá, " ++ nome in
msg

Passo A — Lexer (entrada -> tokens)
- Entrada: exactly the characters in the source (including newlines/spaces)
- Tokens (aprox):
  - `TokenLet` `TokenVarId "nome"` `TokenEquals` `TokenString "Ana"` `TokenIn`
  - `TokenLet` `TokenVarId "msg"` `TokenEquals` `TokenString "Olá, "` `TokenConcat` `TokenVarId "nome"` `TokenIn`
  - `TokenVarId "msg"` EOF

Passo B — Parser (tokens -> AST)
- A primeira `let` vira `Let "nome" (Str "Ana") <rest>`; o `<rest>` é outro `Let`.
- AST (abreviado):
  Let "nome" (Str "Ana")
    (Let "msg" (Concat (Str "Olá, ") (Var "nome"))
         (Var "msg"))

Passo C — TypeChecker (`typeof`) — verificar tipos
- Começa com contexto `[]`.
- `Str "Ana"` → `Just TString`.
- `Let "nome" (Str "Ana") e2` → resulta em checar `e2` com contexto `[("nome", TString)]`.
- `Concat (Str "Olá, ") (Var "nome")` → `Str` e `Var "nome"` tem tipo `TString` no contexto → `Concat` bem-typed → `TString`.
- `Let "msg" ...` adiciona `("msg", TString)` e `Var "msg"` tem `TString`.
- `typecheck` retorna sucesso.

Passo D — Interpreter (`eval`)
- `eval` do `Let` externo: avalia `Str "Ana"` (já valor), substitui `nome` por `"Ana"` no corpo.
- Avalia `Let "msg" ...` similar: avalia `Concat` → concatena strings → `Str "Olá, Ana"`.
- Avalia `Var "msg"` → retorna `Str "Olá, Ana"`.
- Resultado: string "Olá, Ana".

5) Walkthrough completo — exemplo 2 (lambda + aplicação)
Fonte: (README)  (\x : Int -> x + 1) 41

Lexer:
- tokens: `TokenLambda` `TokenVarId "x"` `TokenColon` `TokenTNum` `TokenFun` `TokenVarId ...` etc., `TokenNum 41`

Parser:
- AST: `App (Lam "x" TNum (Add (Var "x") (Num 1))) (Num 41)`

TypeChecker:
- `Lam` annotated with `TNum` → inside body `Add (Var "x") (Num 1)`: `Var "x"` has type `TNum`, `Num 1` `TNum` → Add OK → body `TNum`. So `Lam` type `TFun TNum TNum`.
- `App` checks that function type is `TFun TNum TNum` and argument `Num 41` has `TNum` → result `TNum`.

Interpreter:
- `eval` reduces `App (Lam ...) (Num 41)` — argument is a value, so beta-reduction `subst "x" (Num 41) (Add (Var "x") (Num 1))` → `Add (Num 41) (Num 1)` → `step` → `Num 42`.

6) Semântica operacional (intuição formal)
- Estratégia: call-by-value (CBV). Os princípios:
  - Reduza argumentos antes da aplicação.
  - Beta-reduction só aplicada quando a função é lambda e o argumento é valor.

Pequenas regras (esquemáticas):
- (E-Add)
  - if e1 →* n1 and e2 →* n2 then Add e1 e2 →* n1 + n2
- (E-App)
  - if e1 →* (\x:τ. e) and e2 →* v (v valor) then App e1 e2 →* [x↦v]e
- (E-Let)
  - Let x e1 e2 →* App (\x:_ -> e2) e1 →* ...

Observação: o interpretador implementa isso via `step` (um passo) e `eval` (fechamento recursivo).

7) Regras de tipagem (forma de inferência, notação)
Abaixo apresento regras-chave em estilo natural (SI: sintaxe/semântica informal).

- T-Num
  -------------
  Γ ⊢ n : Int

- T-True / T-False
  -------------
  Γ ⊢ true : Bool

- T-Var
  x:τ ∈ Γ
  ---------
  Γ ⊢ x : τ

- T-Lam
  Γ, x:τ1 ⊢ e : τ2
  --------------------
  Γ ⊢ (\x:τ1 -> e) : τ1 → τ2

- T-App
  Γ ⊢ e1 : τ1 → τ2   Γ ⊢ e2 : τ1
  -----------------------------
  Γ ⊢ e1 e2 : τ2

- T-Add
  Γ ⊢ e1 : Int   Γ ⊢ e2 : Int
  ---------------------------
  Γ ⊢ e1 + e2 : Int

- T-Concat
  Γ ⊢ e1 : String  Γ ⊢ e2 : String
  --------------------------------
  Γ ⊢ e1 ++ e2 : String

- T-If
  Γ ⊢ e1 : Bool  Γ ⊢ e2 : τ  Γ ⊢ e3 : τ
  -------------------------------------
  Γ ⊢ if e1 then e2 else e3 : τ

- T-Proj
  Γ ⊢ e : (τ1, ..., τn)   0 ≤ i < n
  ---------------------------------
  Γ ⊢ proj i e : τ_{i}

Essas regras são a base para o que `typeof` implementa.

8) Como depurar cada etapa (receitas práticas)
- Lexer
  - Objetivo: ver se tokens são os esperados.
  - Técnica: adicione temporariamente `trace`/`print` no início do `lexer` para exibir os tokens produzidos para uma linha de entrada, ou crie uma função `showTokens s = print (lexer s)` no `Main`.
  - O que olhar: escapes de string, ordem de padrões (`==` antes de `=`), keywords mal detectadas.

- Parser
  - Objetivo: obter AST correta.
  - Técnica: gere `Parser.hs` (`happy Parser.y`) e escreva um `main` que chama o parser sobre tokens (`parser (lexer input)`) e imprime a AST (deriving Show nos nós ajuda).
  - O que olhar: produções que reduzem de forma inesperada (aplicação vs operador infix), checar `%left`/`%right`.

- TypeChecker
  - Objetivo: ter mensagens claras de porque algo não typecheckou.
  - Técnica: converta `typeof` temporariamente para `Either String Ty` e retorne mensagens completas (ex.: "expected Int but found String in Add"). Teste com exemplos de falha.

- Interpreter
  - Objetivo: inspecionar passos de avaliação.
  - Técnica: escrever função `traceEval e = trace (show e) (step e)` em GHCI ou intercalar logs em `step` para ver sequência de passos.
  - O que olhar: capturas de variável após `subst`, comportamentos inesperados de `Let` (usa `dummyType`), índices inválidos em `Proj`.

9) Erros comuns e como corrigi-los
- Strings com escapes não suportados.
  - Sintoma: `"He said \"Hi\""` lexado incorretamente ou erro Unclosed String.
  - Corrigir: substituir o trecho do lexer que usa `break` por um loop que parseia escapes (função `lexString` que consome um ``\`` e pega o próximo char como literal).

- Captura de variável na substituição (`subst`).
  - Sintoma: subtituir `x` por `Var "y"` dentro de `Lam "y"` captura `y` do substituto.
  - Corrigir: alpha-renaming -> ao encontrar `Lam v _ body` e `v` aparece em `freeVars(s)`, renomear `v` para um `v'` fresco (gerado com sufixo ou contador), atualizar `body` e prosseguir.

- `TypeChecker` lança `error "Invalid Index"` ao invés de retornar `Nothing`.
  - Sintoma: checker quebra o fluxo e programa falha com exceção.
  - Corrigir: trocar `error` por `Nothing` ou `Left "Invalid index"` para consistência.

- Ambiguidade entre aplicação e operadores infix.
  - Sintoma: `a b + c` pode ser interpretado como `(a b) + c` ou `a (b + c)` dependendo da gramática.
  - Corrigir: reestruturar gramática (introduzir `Atomic`/`App` níveis) e usar precedência para forçar aplicação ter maior prioridade.

10) Extensões práticas (com esboços)
- Escape de aspas e barras — `lexString` esboço:
  - Implementar função recursiva que consome chars:
    - Se vê `\\` e próximo `c`, adicionar `c` ao buffer (interpret escapes `\"`, `\\`, `\n`).
    - Se vê `\"`, termina e retorna string acumulada e resto.
    - Se vê `\n` ou EOF, erro `Unclosed String`.

- Substituição capture-safe (alpha-renaming):
  - Função `freeVars :: Expr -> Set String` para levantar variáveis livres.
  - Ao chamar `subst x s (Lam v tp b)`, se `v == x` fine; else, se `v ∈ freeVars s`, renomear `v` para `v'` (não em `freeVars s` e não em `freeVars b`) e continuar.
  - Use contador global (State monad) para nomes frescos.

- `TypeChecker` com mensagens (Either):
  - Alterar assinatura: `typeof :: Ctx -> Expr -> Either String Ty`.
  - Em cada verificação retornar `Left "expected Int in Add but got ..."` em vez de `Nothing`.

11) Comandos úteis
- Gerar parser (na raiz):

```bash
happy Parser.y
```

- Compilar:

```bash
ghc Main.hs -o main
```

- Executar com arquivo de testes:

```bash
./main < test.txt
```

12) Checklist prático para evoluir o projeto (ordem sugerida)
- [ ] adicionar testes minimalistas cobrindo todos os construtos (test.txt ampliado ou HUnit)
- [ ] implementar `lexString` com escapes
- [ ] tornar `typeof` baseado em `Either String Ty` para melhores erros
- [ ] tornar `subst` capture-safe (alpha-renaming)
- [ ] converter `ctx` para `Map String Ty` se houver muitos bindings
- [ ] documentar convenções e exemplos no `README`

---

Se quiser, eu prossigo com uma das opções concretas:
- implementar `lexString` e atualizar `Lexer.hs` (responda "implementar-escapes");
- transformar `TypeChecker` para `Either String Ty` e ajustar `typecheck` (responda "erro-msgs");
- implementar alpha-renaming para `subst` (responda "alpha-rename");
- ou expandir esse documento com provas formais (regras formais small-step e indução) — responda "formalizar".

Tarefa `EXPLAIN_DEEP.md` criada. Vou marcar a tarefa como completa internamente. 