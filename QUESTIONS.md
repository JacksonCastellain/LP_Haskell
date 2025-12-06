# QUESTIONS: Perguntas e Respostas sobre a implementação

Este arquivo contém duas partes:

1) Um conjunto de 20 questões com resposta e explicação (5 fáceis, 8 médias, 7 difíceis) focadas no código deste repositório (`Lexer.hs`, `Parser.y`, `Parser.hs`, `TypeChecker.hs`, `Interpreter.hs`).

2) Um catálogo por tipo de pergunta (conceitual, leitura de código, bugs comuns, exercícios de extensão, testes/edge cases), com 5–10 exemplos em cada categoria — pronto para usar em avaliações ou estudo.

--

**PARTE 1 — 20 QUESTÕES (RESPOSTAS + EXPLICAÇÕES)**

**Fáceis (5)**

1) Pergunta: O que faz a função `lexer` em `Lexer.hs`?
   - Resposta: Converte a string de entrada em uma lista de `Token`.
   - Explicação: `lexer :: String -> [Token]` aplica padrões por prefixo (por exemplo `'\\'` vira `TokenLambda`, `"..."` vira `TokenString`, números vão para `lexNum`) e chama funções auxiliares (`lexNum`, `lexVarOrKw`).

2) Pergunta: Qual construtor de `Expr` representa uma função anônima com tipo anotado?
   - Resposta: `Lam String Ty Expr`.
   - Explicação: Em `Lexer.hs`/AST, `Lam x tp body` armazena o nome do parâmetro, o tipo `Ty` e o corpo `Expr`.

3) Pergunta: Em `Parser.y`, como é representada a gramática para uma expressão `let`?
   - Resposta: `"let" varid '=' Exp "in" Exp  { Let $2 $4 $6 }`.
   - Explicação: A produção consome a keyword `let`, um identificador, `=`, uma expressão, `in` e outra expressão — e constrói `Let var e1 e2` com os componentes relevantes.

4) Pergunta: O que `typeof ctx (Num _)` retorna em `TypeChecker.hs`?
   - Resposta: `Just TNum`.
   - Explicação: Números recebem tipo `TNum` independentemente do contexto.

5) Pergunta: Qual função executa a avaliação completa (redução até valor) no interpretador?
   - Resposta: `eval` em `Interpreter.hs`.
   - Explicação: `eval` chama `step` recursivamente até obter um valor (ou trata `Let` de forma especial); `step` faz um passo de redução.

**Médias (8)**

6) Pergunta: Como `lexVarOrKw` diferencia keywords de identificadores?
   - Resposta: Usa `span` para extrair um lexema (letras/dígitos) e faz `case` sobre strings específicas como "true", "if", "let" etc.; se não bater em nenhum keyword, retorna `TokenVarId ident`.
   - Explicação: `span` retorna `(ident, rest)`, e a correspondência linear de strings no `case` detecta keywords; a ordem não é crítica aqui, mas facilita tratamento de palavras reservadas.

7) Pergunta: Por que `Parser.y` declara `%left '+' '-'` e `%left '*'`? Isso não faz `*` ter menor precedência que `+`? 
   - Resposta: A ordem das diretivas define precedência — diretivas declaradas mais abaixo têm maior precedência no Happy; no entanto, a gramática também organiza produções, e `'*'` foi colocada em sua própria diretiva para controlar associatividade.
   - Explicação: Em Happy, especificar precedência ajuda resolver conflitos shift/reduce. A combinação de `%left` e a estrutura de produções define as regras desejadas.

8) Pergunta: O que acontece quando o `lexer` encontra uma string não terminada antes do `\n`?
   - Resposta: Lança erro com `error "Unclosed String"`.
   - Explicação: A lógica do lexer usa `break` até `"` ou `\n`; se `rest` começar por `\n` ou acabar inesperadamente, entra no caso de erro.

9) Pergunta: Em `TypeChecker.hs`, como é tipada a aplicação `App e1 e2`?
   - Resposta: `e1` deve ter tipo `TFun tp tr`; então `e2` deve ter tipo `tp`; resultado tem tipo `tr`.
   - Explicação: `typeof` primeiro checa `typeof ctx e1`, casando com `TFun tp tr`; depois compara `typeof ctx e2` com `tp`.

10) Pergunta: Como `Interpreter.step` implementa beta-reduction para `App`?
    - Resposta: Se a função é `Lam x tp e1` e o argumento `e2` é valor (`isValue e2`), então `subst x e2 e1` é aplicado; caso contrário, reduz argumento ou função conforme ordem.
    - Explicação: Implementa avaliação call-by-value (estratégia de reduzir argumento antes de substituí-lo).

11) Pergunta: O que `Tuple [Expr]` representa e como a projeção `Proj` funciona?
    - Resposta: `Tuple` representa um tuple literal; `Proj i e` acessa o i-ésimo elemento após a tupla ser avaliada.
    - Explicação: `step (Proj i (Tuple es))` retorna `es !! i` (com verificação de índice), enquanto `Proj i e` reduz `e` até tornar uma tupla.

12) Pergunta: Por que `TypeChecker` usa `Maybe Ty` em vez de lançar erros direto?
    - Resposta: `Maybe Ty` permite tratar falha de tipagem de forma pura (retornando `Nothing`) em vez de `error`, dando flexibilidade para lidar com erros ou propagá-los.
    - Explicação: A função auxiliar `typecheck` transforma `Nothing` em `error "Type error!"` quando necessário.

13) Pergunta: Onde e por que `TokenConcat` (`++`) e `TokenLength` existem? Como são tratados?
    - Resposta: São tokens para concatenação de strings e função `length`. `Parser.y` possui produções `Exp "++" Exp { Concat $1 $3 }` e `"length" Exp { Length $2 }` e `TypeChecker`/`Interpreter` tratam `Concat` e `Length` especificamente.
    - Explicação: `TypeChecker` exige `TString` para `Concat`, e `Interpreter` concatena strings quando ambos são `Str`.

**Difíceis (7)**

14) Pergunta: A substituição (`subst`) em `Interpreter.hs` é capture-safe? Explique uma situação problemática.
    - Resposta: Não completamente — o `subst` evita substituir dentro de um `Lam` se o parâmetro tem mesmo nome, mas não faz renomeação de variáveis livres que poderiam ser capturadas. Ex.: substituição de `x` por `Var "y"` em `Lam "y" _ (Var "x")` pode capturar livremente.
    - Explicação: Para ser capture-safe, seria necessário renomear parâmetros do `Lam` (alpha-renaming) quando houver colisão entre `x` (a variável a substituir) e variáveis livres do substituto `s`.

15) Pergunta: Identifique um caso em que o parser gerado (`Parser.hs`) poderia produzir um conflito shift/reduce com a produção `Exp Exp { App $1 $2 }`. Como a gramática evita ambiguidade da aplicação vs operadores infix?
    - Resposta: A produção `Exp Exp` (aplicação) é ambígua com operadores infix se não controlada; Happy usa precedência/associatividade e a ordem das produções para resolver. A ambiguidade surge entre reduzir `Exp '+' Exp` ou interpretar `Exp Exp` como primeiro `Exp` sendo uma expressão composta.
    - Explicação: A diretiva de precedência e a colocação das produções ajudam o gerador a preferir certos parses; no entanto, gramáticas com aplicação e operadores costumam exigir cuidado (por exemplo, tornar a aplicação mais forte via produção ou inserindo `Atom` não ambígua).

16) Pergunta: Proponha uma modificação em `Lexer.hs` que permita escape de aspas dentro de strings (por exemplo `"He said \"Hi\""`). Dê um esboço de implementação.
    - Resposta: Em vez de `break (\c -> c == '"' || c == '\n')`, parsear caractere a caractere reconhecendo `\` seguido por `"` como parte do conteúdo. Exemplo: escrever uma função `lexString` que consome caracteres: quando vê `\` pega o próximo char (incluindo `"`) e o acrescenta ao buffer; quando vê `"` termina; quando vê `\n` ou EOF lança erro.
    - Explicação: `break` não suporta escapes; um loop explícito permite acumular conteúdo com tratamento de `\`.

17) Pergunta: Analise como `TypeChecker` lida com projeção em uma tupla aninhada e identifique um possível ponto de falha.
    - Resposta: `typeof` de `Proj i e` verifica se `typeof ctx e` é `TTuple ts` e retorna `Just (ts !! i)` se índice válido, senão `error "Invalid Index"`. Ponto de falha: se `e` tiver tipo `TTuple` mas com comprimento menor que `i`, `error` é lançado (em vez de `Nothing`), tornando a função parcial.
    - Explicação: Melhor abordagem: retornar `Nothing` quando índice inválido para manter totalidade.

18) Pergunta: Existe risco de não-terminação nas funções `eval`/`step`? Dê um exemplo de termo que diverge.
    - Resposta: Sim. Exemplo clássico: `let x = x in x` ou definição recursiva sem fix-point seguro pode levar à não-terminação ou erro de redução. Outro exemplo em lambda: `App (Lam x _ (App (Var x) (Var x))) (Lam x _ (App (Var x) (Var x)))` (omega combinator) diverge.
    - Explicação: Linguagem tem avaliação por valor e permite expressões que não reduzem a valor; o interpretador não tem verificação de terminação.

19) Pergunta: O `TypeChecker` permite inferência de tipos automáticos? Se não, qual alteração permitiria inferência limitada?
    - Resposta: Não — `Lam` requer anotação de tipo (`'\' varid ':' Type "->" Exp`). Para inferência limitada, poderíamos implementar Hindley-Milner (unificação) ou um sistema mais simples de inferência local, adicionando variáveis de tipo e unificação em `typeof`.
    - Explicação: Implementar HM é significativo: introduziria variáveis tipo, geração de constraints e resolução por unificação.

20) Pergunta: Suponha que `lexVarOrKw` devolva `TokenVarId "in"` (ou seja, falhe ao reconhecer `in`): que efeitos isso tem no parser e no comportamento do programa?
    - Resposta: A produção `let varid '=' Exp "in" Exp` depende de `TokenIn`; se `in` for tokenizado como `TokenVarId "in"`, o parser não reconhecerá a produção `let ... in ...` e provavelmente lançará erro de sintaxe. Em programas onde `in` aparece como palavra chave, serão tratados como variável, mudando semântica e causando parse errors.
    - Explicação: Keywords devem ser tokenizadas separadamente; lexers incorretos quebram a gramática.

--

**PARTE 2 — CATEGORIAS DE PERGUNTAS (5–10 EXEMPLOS CADA)**

Abaixo, por categoria, proponho perguntas úteis para estudo, avaliação ou exame prático. Cada item inclui uma sugestão de resposta curta ou indicação do que se espera.

**A) Conceitual (5 exemplos)**
1. O que é um lexer e qual seu papel no pipeline de compilação/interpretador? 
   - Esperado: separa a entrada em tokens, abstraindo caracteres em unidades sintáticas.
2. Explique a diferença entre parser LR (como Happy) e parser por combinators (Megaparsec).
   - Esperado: LR é bottom-up, gerador de tabelas; combinators são top-down recursivos, mais flexíveis.
3. O que significa "call-by-value" e onde aparece na implementação?
   - Esperado: argumentos são avaliados antes da aplicação; `Interpreter.step` aplica `subst` apenas quando `isValue e2`.
4. O que é alpha-renaming e por que é importante na substituição?
   - Esperado: renomear variáveis ligadas para evitar captura; importante para `subst` correta.
5. O que é associatividade e precedência de operadores e como o arquivo `.y` as expressa?
   - Esperado: usam `%left`/`%right` e a ordem das diretivas para resolver conflitos.

**B) Leitura de código (5 exemplos)**
1. Explique em uma frase o que faz `lexNum` em `Lexer.hs`.
2. Localize no `Parser.y` a produção que cria `App` e explique os símbolos `$1` e `$2`.
3. Qual ramo de `Interpreter.step` trata `Eq` entre strings? Copie-o e explique.
4. Dado `TypeList` no `Parser.y`, o que `$1 : $3` representa na ação semântica?
5. Onde `TypeChecker` trata `Concat` e qual condição ele impõe nos operandos?

**C) Debugging / Bugs comuns (6 exemplos)**
1. Bug: lexer aceita strings multilinha (aceita `\n` dentro do literal) — identifique causa e correção.
   - Resposta: `break` para `\n` detecta `\n` como terminador e lança erro; se está aceitando então `break` foi substituído. Correção: garantir que `rest` seja testado para `\n` e que erro seja lançado.
2. Bug: `subst` causa captura de variável — dê exemplo e correção (alpha-renaming).
3. Bug: `TypeChecker` lança `error "Invalid Index"` em vez de `Nothing` — proposta de correção: retornar `Nothing` quando índice inválido.
4. Bug: `Parser` falha para aplicação encadeada sem parênteses — investigar precedência/produção `Exp Exp`.
5. Bug: lexer confunde `==` com `=` — explique prioridade de padrões (`'=':'='` deve vir antes de `'='`).
6. Bug: `Interpreter.printPretty` não mostra tipos nas lambdas; é apenas estético — correção simples de formatação.

**D) Exercícios de extensão / implementação (7 exemplos)**
1. Implementar escape de aspas em strings no `lexer` (veja pergunta 16 acima).
2. Substituir `happy`/`Parser.y` por `megaparsec` (implemente `Parser.hs` em estilo combinator) — passos e exemplos.
3. Tornar `subst` capture-safe com alpha-renaming — implementar função de geração de nomes frescos.
4. Estender a linguagem com `letrec` (recursão) e adaptar `Interpreter`/`TypeChecker`.
5. Adicionar listas como tipo nativo (`[T]`) com sintaxe `[...]` e operações `head`, `tail`.
6. Implementar inferência de tipos simples (variáveis de tipo e unificação) — esboço de algoritmo.
7. Adicionar suporte a comentários no lexer (linha `--` e bloco `{- -}`) e ajustar testes.

**E) Testes e edge-cases (5 exemplos)**
1. Teste: `proj 0 (10, 20)` deve retornar `10` — escrever teste com `test.txt`.
2. Edge-case: `length ""` deve retornar `0` — escrever caso de unit test.
3. Teste de erro: `proj 5 (1,2)` deve provocar erro de índice (especificar se esperar `error` ou `Nothing`).
4. Teste de tipo: `(
\x : Int -> x) true` deve falhar no `TypeChecker`.
5. Stress-test: expressão grande com 1000 aplicações — medir consumo de pilha/tempo no interpretador.

--

Observações finais:
- Posso adaptar o escopo (ex.: gerar arquivo PDF com questões, criar versão em português/inglês separada, ou criar um conjunto de testes automatizados usando `test.txt`).
- Quer que eu atualize o `README.md` com link para `QUESTIONS.md` e `EXPLAIN.md`? Responda "linkar" se quiser que eu faça isso.

Arquivo criado automaticamente: `QUESTIONS.md`.
