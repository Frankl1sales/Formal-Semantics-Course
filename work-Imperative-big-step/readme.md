### Definição das Árvores Sintáticas

A representação dos programas é feita usando árvores sintáticas abstratas (ASTs), que são fundamentais para capturar a estrutura de um programa de forma hierárquica. Em Haskell, essas árvores são definidas como tipos de dados para representar as diferentes construções da linguagem:

- **Expressões Aritméticas (E):** Este tipo de dado representa números, variáveis e operações aritméticas, como soma, subtração, multiplicação e divisão. Essas expressões formam a base para cálculos numéricos dentro do programa.

- **Expressões Booleanas (B):** Este tipo de dado lida com valores booleanos (`TRUE` e `FALSE`), operações lógicas (`Not`, `And`, `Or`) e comparações (`Leq` para "menor ou igual", `Igual` para verificar igualdade entre expressões aritméticas). São usadas principalmente em condições e controle de fluxo.

- **Comandos (C):** Este tipo de dado abrange as instruções que alteram o estado do programa, como loops (`While`, `DoWhile`, `Loop`), condicionais (`If`, `Unless`), atribuições de variáveis, sequenciamento de comandos (`Seq`), e operações específicas como `Swap` e dupla atribuição. Eles definem a lógica de execução do programa.

### Manipulação de Memória

A memória é essencial para o estado de um programa, armazenando valores associados a variáveis. Em Haskell, a memória pode ser modelada como uma lista de pares, onde cada par associa o nome de uma variável ao seu valor. As principais operações de manipulação de memória incluem:

- **Procura de Variáveis:** Uma função que recebe a memória e o nome de uma variável, retornando o valor associado a essa variável. Se a variável não existir na memória, é necessário definir como esse caso será tratado (por exemplo, retornando um valor padrão ou gerando um erro).

- **Modificação de Variáveis:** Uma função que altera o valor de uma variável existente na memória, retornando uma nova versão da memória com a atualização aplicada. Isso é essencial para refletir as mudanças feitas durante a execução de um programa.

### Avaliação Semântica (Big-Step)

A semântica operacional dos programas, ou seja, como eles são executados e como seus resultados são obtidos, é definida por meio de funções de avaliação. Essas funções aplicam as regras de execução sobre as expressões e comandos definidos nas árvores sintáticas:

- **Expressões Aritméticas:** A função de avaliação para expressões aritméticas calcula o valor numérico resultante com base na operação especificada. Por exemplo, avaliar uma soma retorna a soma dos valores das duas expressões aritméticas envolvidas.

- **Expressões Booleanas:** A função de avaliação para expressões booleanas determina se uma condição é verdadeira ou falsa, com base nas operações lógicas e comparações definidas na árvore sintática.

- **Comandos:** A função de avaliação para comandos processa instruções como loops, condicionais e atribuições, modificando a memória conforme necessário e retornando o estado final da memória após a execução. Essa função segue as regras da semântica operacional, garantindo que os comandos sejam executados corretamente.

### Tratamento de Condições e Loops

O código inclui o tratamento de estruturas de controle, como condicionais e loops, que são essenciais para controlar o fluxo de execução do programa:

- **If-Then-Else:** Executa um bloco de comandos se uma condição booleana for verdadeira e outro bloco se for falsa.

- **Unless:** Executa o bloco de comandos apenas se a condição for falsa, invertendo a lógica de `If`.

- **While:** Executa repetidamente um bloco de comandos enquanto uma condição booleana for verdadeira.

- **DoWhile:** Similar ao `While`, mas garante que o bloco seja executado pelo menos uma vez antes de verificar a condição.

- **Loop:** Executa um bloco de comandos por um número fixo de vezes, conforme definido por uma expressão aritmética.

### Operações Especiais

Além dos comandos básicos, a linguagem suporta operações especiais para manipular a memória e o estado do programa de maneiras mais avançadas:

- **Seq:** Representa a execução sequencial de comandos.

- **Skip:** Um comando vazio que não altera o estado da memória.

- **Swap:** Troca os valores de duas variáveis na memória.

- **Double Assignment:** Permite a atribuição simultânea de novos valores a duas variáveis.

### Exemplos de Programas

Para validar e demonstrar o funcionamento das definições anteriores, foram criados exemplos de programas:

- **Expressões Aritméticas:** Um exemplo que demonstra a avaliação de operações aritméticas simples.

- **Expressões Booleanas:** Exemplos que utilizam comparações e operações lógicas para avaliar condições.

- **Programas Imperativos:** Exemplos de programas que manipulam variáveis e controlam o fluxo de execução com loops e condicionais.

- **Operações Específicas:** Exemplos que demonstram o uso de comandos avançados, como loops e atribuições duplas.

### Conclusão

Com a adição das definições de árvores sintáticas abstratas, o modelo de execução se torna mais robusto, permitindo uma representação clara e eficiente dos programas. As ASTs permitem que os programas sejam decompostos em seus componentes fundamentais, facilitando a avaliação semântica e a manipulação da memória. Isso fornece uma base sólida para a execução de programas imperativos dentro de um ambiente funcional como Haskell, permitindo a simulação de um ambiente de programação estruturada.


*Main> let memoriaInicial = [("x", 5), ("y", 0)]
*Main> let resultado = cbigStep (fatorial, memoriaInicial)
*Main> print resultado


$ ghci
GHCi, version 9.4.4: https://www.haskell.org/ghc/  :? for help
Prelude> :load program.hs
[1 of 1] Compiling Main             ( program.hs, interpreted )
Ok, one module loaded.
*Main> -- Agora você pode testar suas funções e exemplos
*Main> ebigStep (progExp1, exSigma)
13
*Main> ebigStep (progExp1, exSigma2)
6
*Main> bbigStep (teste1, exSigma)
True
*Main> bbigStep (teste2, exSigma)
False
*Main> cbigStep (fatorial, exSigma)
(Skip,[("x",1),("y",6),("z",0)])


*Main> cbigStep (fatorial, [("x", 5), ("y", 1)])
(Skip,[("x",1),("y",120)])


