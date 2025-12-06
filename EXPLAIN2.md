# EXPLAIN2: Visão macro do projeto LP_Haskell

Este documento explica o projeto em alto nível, descrevendo o propósito e o papel de cada componente (`Lexer.hs`, `Parser.y`/`Parser.hs`, `TypeChecker.hs`, `Interpreter.hs`, `Main.hs`), o fluxo de dados entre eles, decisões de design principais, pontos de atenção para manutenção/depuração, e sugestões de próximas etapas.

Objetivo do projeto
- Implementar um interpretador minimalista para uma linguagem funcional pequena com inteiros, booleanos, strings, tuplas, funções anotadas com tipos, `let`, `if`, concatenação e projeção.
- Abranger o pipeline clássico de linguagem: tokenização (lexer), parsing (Happy), verificação de tipos e execução (interpreter).

Visão geral do pipeline (fluxo de dados)
1. Entrada textual (arquivo ou stdin) — programa escrito na linguagem alvo.
2. Lexer (`Lexer.hs`) — transforma a sequência de caracteres em tokens (`[Token]`).
   - Saída: lista de tokens com literais (números, strings) e keywords/símbolos.
3. Parser (`Parser.y` + `Parser.hs` gerado por Happy) — consome `Token` e constrói uma AST (`Expr`).
   - Saída: `Expr` (árvore sintática abstrata) que representa semanticamente o programa.
4. TypeChecker (`TypeChecker.hs`) — percorre a AST verificando tipos e anotando/validando coerência.
   - Retorna `Just Ty` (se bem tipado) ou `Nothing` (se erro); `typecheck` lança um erro para `Nothing`.
5. Interpreter (`Interpreter.hs`) — avalia a AST tipada, reduzindo expressões até valores e produzindo saída.
   - Funções importantes: `eval` (avaliação completa), `step` (um passo), `subst` (substituição de variáveis), `printPretty` (impressão do resultado).
6. Saída textual — resultado final impresso.

Papel e relevância de cada componente
- Lexer (`Lexer.hs`)
  - Papel: isolar detalhes de caracteres (espaços, escapes, formatação) e identificar unidades tokenizadas (números, strings, `let`, operadores).
  - Relevância: mantém o parser livre de detalhes de baixo nível; facilita suporte a novas literais e comentários centralizando regras de tokenização.
  - Pontos de atenção: tratamento de strings (escapes), ordem dos padrões (ex.: `==` antes de `=`), keywords vs identificadores.

- Parser (`Parser.y` / `Parser.hs`)
  - Papel: transformar tokens em uma representação estrutural (`Expr`) com significado sintático e semântico.
  - Relevância: implementa precedência/associatividade e constrói AST com ações semânticas (`{ Add $1 $3 }`).
  - Pontos de atenção: ambiguidade entre aplicação (`Exp Exp`) e operadores infix; uso de diretivas `%left`/`%right` no Happy; manter `Parser.y` sincronizado com `Parser.hs` gerado.

- TypeChecker (`TypeChecker.hs`)
  - Papel: garantir que expressões façam sentido em termos de tipos (ex.: somar apenas números, concatenar strings); implementa as regras de tipagem da linguagem.
  - Relevância: evita erros em tempo de execução (ou os detecta anteriormente), define invariantes que o `Interpreter` pode assumir.
  - Pontos de atenção: escolhas de design (retornar `Maybe Ty` vs `Either String Ty`), tratamento de index-out-of-bounds em `Proj` (aqui lança `error`), ausência de inferência de tipos (lam requires annotations).

- Interpreter (`Interpreter.hs`)
  - Papel: executar a semântica operacional da linguagem (call-by-value) — realiza passos de redução e avalia expressões.
  - Relevância: implementa o comportamento observável da linguagem; central para testar semântica e desempenho.
  - Pontos de atenção: `subst` não é capture-safe; `step` pode lançar erros para casos inesperados; `eval` é recursivo e pode causar stack overflow em termos grandes ou recursivos.

Decisões de design importantes (resumo)
- Abordagem: implementador optou por uma implementação didática e direta — código legível, poucas abstrações avançadas. Isso torna o projeto ideal para aprendizado, porém menos robusto em produção.
- Tipos: tipagem explícita em `Lam` e checagem simples com `Maybe Ty`. Escolha favorece simplicidade sobre ergonomia (sem inferência nem mensagens de erro detalhadas).
- Parser-generator vs combinators: o autor usa Happy (LR parsing) — bom para gramáticas mais complexas e desempenho previsível; alternativas são parser combinators (Megaparsec) para implementação mais integrada em Haskell.
- Substituição: `subst` é simples mas carece de alpha-renaming — suficiente para exemplos, mas perigoso para extensões com variáveis frescas e geração automática de nomes.

Como depurar e testar (prático)
- Testes unitários:
  - Crie expressões AST diretamente e verifique `typeof [] e` e `eval e` (teste `printPretty` do resultado).
  - Exemplos: `Add (Num 1) (Num 2)` → tipo `TNum`, eval `Num 3`.
- Testes integrados:
  - Escreva casos em `test.txt` e execute `./main < test.txt` após `happy` + `ghc`.
- Depuração do lexer:
  - Adicione prints temporários no `lexer` ou escreva casos de teste que imprimam tokens para entradas representativas.
- Depuração do parser:
  - Gere `Parser.hs` e inspecione `happyReduce_*`; use exemplos simples e verifique a AST resultante.
- Depuração do typechecker:
  - Substitua `typecheck` temporariamente por versões que usam `Either String Ty` para mensagens mais explícitas.

Roadmap de melhorias (priorizado)
1. Mensagens de erro no typechecker: trocar `Maybe Ty` por `Either String Ty` e propagar mensagens.
2. Tornar `subst` capture-safe com alpha-renaming (gerar nomes frescos).
3. Suporte a escapes em strings (suportar `\"`, `\\`, `\n` corretamente) no `lexer`.
4. Trocar `ctx :: [(String, Ty)]` por `Map String Ty` para eficiência e clareza.
5. Implementar inferência de tipos (Hindley-Milner) como etapa opcional.
6. Adicionar testes automatizados (HUnit/QuickCheck) e pipeline CI.
7. Documentação do design e convenções de codificação, com exemplos de entrada/saída no `README`.

Tarefas imediatas recomendadas
- Corrigir `Lexer.hs` para suportar escapes em strings e separar lógica de lexing em funções menores (`lexString`, `lexOp`, etc.).
- Atualizar `TypeChecker` para evitar `error` em `Proj` e retornar `Nothing` ou `Left "Index out of bounds"`.
- Documentar no `README.md` como reproduzir o build e executar testes (com comandos `happy`/`ghc`).

Arquitetura e conexões conceituais
- O projeto segue arquitetura clássica: front-end (lexer+parser), middle-end (type checking / AST), back-end (interpreter). Essa separação facilita trocar componentes (por exemplo, usar Alex para lexer ou Megaparsec para parser) sem reescrever o restante.
- A AST (`Expr`) funciona como contrato entre componentes: quaisquer mudanças em `Expr` exigem atualizações no `Lexer`, `Parser.y`, `TypeChecker` e `Interpreter`.

Se quiser, eu:
- aplico as melhorias básicas (ex.: `lexer` com escapes + `Either String Ty` no `TypeChecker`) e crio PRs locais com patches (responda "implementar melhorias");
- ou apenas expando `EXPLAIN.md` com o mesmo nível de detalhe macro por arquivo (responda "mais macro").

Tarefa `EXPLAIN2.md` criada na raiz do repositório.
