PROJ_TUPLE — Tuplas e Projeção (Tuple / Proj)

Objetivo
- Fornecer uma referência focada e aprofundada sobre tuplas (`Tuple`) e projeção (`Proj`) presente na linguagem do repositório.
- Justificar decisões de implementação, descrever regras de tipagem e semântica, mostrar exemplos passo-a-passo e propor melhorias e testes.

Sumário
- Sintaxe e tokens
- Representação no AST
- Regras de tipagem (TypeChecker)
- Semântica operacional (Interpreter / step)
- Exemplos de redução (passo-a-passo)
- Casos-limite e tratamento de erros
- Alternativas de design e justificativas
- Testes sugeridos e casos de unidade
- Patches e sugestões de melhoria

1. Sintaxe e tokens
- Síntaxe comum vista no `Parser.y` (exemplos):
  - Tupla literal: `( e1 , e2 , ..., en )` — elementos separados por `,` entre parênteses.
  - Projeção: `proj i e` ou em algumas sintaxes `e . i` (no repositório atual usar `proj` conforme gramática).

- Tokens relevantes (do `Lexer.hs`):
  - `TokenLPar` / `TokenRPar` — `(` e `)`
  - `TokenComma` — `,` (separador da lista)
  - `TokenVarId` / `TokenNum` / `TokenString` — elementos da tupla
  - `TokenProj` ou palavra-chave `proj` (depende da gramática)

Justificativa: manter `,` e parênteses como tokens permite distinguir tupla de expressão entre parênteses (ex.: `(e)` vs `(e1,e2)`). A gramática do parser deve diferenciar o caso de tupla única (que geralmente é apenas `e` entre parênteses) e o caso de tupla de aridade >1.

2. Representação no AST
- Estrutura típica no repositório (exemplo inspirado em `Expr`):
  - `ETuple [Expr]` — lista de expressões como elementos da tupla.
  - `EProj Int Expr` — projeção do índice `Int` (geralmente 1-based ou 0-based — ver abaixo) sobre a expressão `Expr`.

Justificativa: usar uma lista para `ETuple` é simples e flexível, permitindo tuplas de aridade dinâmica. `EProj` carrega o índice concreto para facilitar checagem e avaliação.

Índice base: É fundamental decidir se `Proj` usa indexação 0-based (comum em implementações internas) ou 1-based (às vezes mais legível para usuários). A implementação deve documentar e tratar consistentemente: verificar `Parser.y` e `TypeChecker.hs` para confirmar convenção. Se as funções de projeção chamam `error "Invalid Index"` com base em `i-1`, é sinal de indexação 1-based.

3. Regras de tipagem (TypeChecker)
Regra informal (para uma tupla com n componentes):
- Se para cada i, C |- ei : Ti, então C |- (e1,...,en) : (T1,...,Tn)

Regra de projeção:
- Se C |- e : (T1,...,Tn) e 1 <= i <= n, então C |- proj i e : Ti
- Caso contrário (índice fora do intervalo ou `e` não é tupla), produzir erro de tipo.

Implementação atual (observações):
- O `TypeChecker.hs` aparenta usar `Maybe Ty` para `typeof`. Em `Proj` costuma-se:
  - verificar `typeof ctx e`;
  - casar o resultado com `TTuple tys` (ou equivalente);
  - checar se o índice está dentro de `length tys` e retornar `Just (tys !! (i-1))` (ou `i` no deslocamento correto);
  - se falhar, retornar `Nothing`.

Justificativa: retornar `Nothing` representa erro de tipagem; porém, `Either String Ty` é mais informativo (ver seção "Melhorias" abaixo). Lançar `error` em tempo de tipo (p. ex., quando index fora do intervalo) é perigoso — melhor sinalizar erro para o chamador.

4. Semântica operacional (Interpreter / step)
- Avaliação de tupla (`ETuple`):
  - Política comum: avaliar os elementos do vetor da esquerda para a direita até que todos sejam valores (CBV). O valor da tupla é `VTuple [v1,...,vn]`.
  - Ex.: `step` aplica regras de congruência para avaliar elementos um por um.

- Avaliação de `Proj` (`EProj i e`):
  - Primeiro, avaliar `e` até que seja valor.
  - Se `e` for uma tupla valor `VTuple vs` e índice `i` for válido, o passo reduz para `vs !! (i-1)` (ou `i`, conforme convencionado).
  - Se `e` não for uma tupla valor, regra de erro (ou stuck) — o interpretador pode lançar erro runtime ou tratar como erro de execução.

Observações sobre `Let` transformado: no repositório, `Let` às vezes é transformado em aplicação (`(\