# LP Haskell — Interpretador Funcional em Haskell

![Logo](./logo.svg)

Um interpretador minimalista escrito em Haskell com as etapas clássicas de front-end e back-end de linguagem: **lexer**, **parser** (Happy), **type checker** e **interpreter**. O projeto suporta inteiros, booleanos, strings, tuplas, funções, `let`, `if`, concatenação de strings, e projeção de tuplas.

---

## Estrutura do projeto
```arduino
LP_Haskell/
├── Lexer.hs -- Tokenização
├── Parser.y -- Gramática (Happy)
├── Parser.hs -- Parser gerado por Happy
├── TypeChecker.hs -- Verificação de tipos
├── Interpreter.hs -- Avaliação da AST
├── Main.hs -- Encadeamento do pipeline (stdin -> stdout)
├── test.txt -- testes
├── entraces.txt -- entradas válidas
└── logo.svg

```

---

## Como usar (local)

### Instalar as ferramentas (usando `ghcup`)
```bash
# instale ghcup se ainda não tem (https://www.haskell.org/ghcup/)
# depois:
ghcup install ghc 9.6.7
ghcup set ghc 9.6.7
ghcup install cabal
ghcup install happy
cabal update
```

### Gerar parser e compilar

```bash
happy Parser.y
ghc Main.hs -o main
./main < test.txt

```
## Exemplos
### 1 — Let + String concat

##### Entrada:

```haskell
let nome = "Ana" in
let msg  = "Olá, " ++ nome in
msg

```

##### Saída 

```arduino
"Olá, Ana"

```

### 2 — Função anônima

```haskell
(\x : Int -> x + 1) 41

```

##### Saída 

```arduino
42

```

### 3 — Projeção de tupla

```haskell
proj 0 (10, 20, 30)

```

##### Saída 

```arduino
10

```



