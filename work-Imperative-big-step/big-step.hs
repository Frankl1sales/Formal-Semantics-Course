
-- Definição das árvore sintática para representação dos programas:

data E = Num Int
      |Var String
      |Soma E E
      |Sub E E
      |Mult E E
      |Div E E
   deriving(Eq,Show)

data B = TRUE
      | FALSE
      | Not B
      | And B B
      | Or  B B
      | Leq E E    -- menor ou igual
      | Igual E E  -- verifica se duas expressões aritméticas são iguais
   deriving(Eq,Show)

data C = While B C
    | If B C C
    | Seq C C
    | Atrib E E
    | Skip
    | DoWhile C B      ---- Do C While B: executa C enquanto B avalie para verdadeiro
    | Unless B C C   ---- Unless B C1 C2: se B avalia para falso, então executa C1, caso contrário, executa C2
    | Loop E C    --- Loop E C: Executa E vezes o comando C
    | Swap E E --- recebe duas variáveis e troca o conteúdo delas
    | DAtrrib E E E E -- Dupla atribuição: recebe duas variáveis "e1" e "e2" e duas expressões "e3" e "e4". Faz e1:=e3 e e2:=e4.
   deriving(Eq,Show)                


-----------------------------------------------------
-----
----- As próximas funções, servem para manipular a memória (sigma)
-----
------------------------------------------------


--- A próxima linha de código diz que o tipo memória é equivalente a uma lista de tuplas, onde o
--- primeiro elemento da tupla é uma String (nome da variável) e o segundo um Inteiro
--- (conteúdo da variável):


-- Definição do tipo Memoria
type Memoria = [(String, Int)]

-- Definição da memória inicial
memoriaInicial :: Memoria
memoriaInicial = [("x", 10), ("y", 0),("w", 5)]  -- A variável "y" inicia com o valor 0

-- Memória exemplo exSigma
exSigma :: Memoria
exSigma = [ ("x", 10), ("temp",0), ("y",0)]

exSigma2 :: Memoria
exSigma2 = [("x",3), ("y",1), ("z",0)]

-- para executar em minExample
exSigma3 :: Memoria
exSigma3 = [("a", 5), ("b", 10), ("min", 0)]


--- A função procuraVar recebe uma memória, o nome de uma variável e retorna o conteúdo
--- dessa variável na memória. Exemplo:
---


-- | A função 'procuraVar' busca o valor de uma variável no estado de memória.
-- *Main> procuraVar exSigma "x"
--- *Main> procuraVar exSigma "x"
--- 10

procuraVar :: Memoria -> String -> Int
procuraVar [] s = error ("Variavel " ++ s ++ " nao definida no estado")
procuraVar ((s,i):xs) v
  | s == v     = i
  | otherwise  = procuraVar xs v


--- A função mudaVar, recebe uma memória, o nome de uma variável e um novo conteúdo para essa
--- variável e devolve uma nova memória modificada com a varíável contendo o novo conteúdo. A
--- chamada
---
--- *Main> mudaVar exSigma "temp" 20
--- [("x",10),("temp",20),("y",0)]
---
---
--- essa chamada é equivalente a operação exSigma[temp->20]

mudaVar :: Memoria -> String -> Int -> Memoria
mudaVar [] v n = error ("Variavel " ++ v ++ " nao definida no estado")
mudaVar ((s,i):xs) v n
  | s == v     = ((s,n):xs)
  | otherwise  = (s,i): mudaVar xs v n


-------------------------------------
---
--- Completar os casos comentados das seguintes funções:
---
---------------------------------



-- A função `ebigStep` avalia expressões aritméticas no contexto de uma memória dada.
-- Ela recebe um par (E, Memoria), onde `E` representa a expressão a ser avaliada e `Memoria` é o estado atual das variáveis.
ebigStep :: (E,Memoria) -> Int
ebigStep (Var x,s) = procuraVar s x
ebigStep (Num n,s) = n
ebigStep (Soma e1 e2,s)  = ebigStep (e1,s) + ebigStep (e2,s)
--ebigStep (Sub e1 e2,s)  = (subtração)
--ebigStep (Mult e1 e2,s)  = (multiplicação)
-- ebigStep(Div e1 e2,s)
ebigStep (Sub e1 e2, s) = ebigStep (e1, s) - ebigStep (e2, s)
ebigStep (Mult e1 e2, s) = ebigStep (e1, s) * ebigStep (e2, s)
ebigStep (Div e1 e2, s) = ebigStep (e1, s) `div` ebigStep (e2, s)

-- A função `bbigStep` avalia expressões booleanas no contexto de uma memória dada.
-- Ela recebe um par (B, Memoria), onde `B` representa a expressão boolean e `Memoria` é o estado atual das variáveis.
bbigStep :: (B,Memoria) -> Bool
bbigStep (TRUE,s)  = True
bbigStep (FALSE,s) = False
bbigStep (Not b,s) 
   | bbigStep (b,s) == True     = False
   | otherwise                  = True 
--bbigStep (And b1 b2,s )  =
--bbigStep (Or b1 b2,s )  =
--bbigStep (Leq e1 e2,s) =
--bbigStep (Igual e1 e2,s) = -- recebe duas expressões aritméticas e devolve um valor booleano dizendo se são iguais
bbigStep (And b1 b2, s) = bbigStep (b1, s) && bbigStep (b2, s)
bbigStep (Or b1 b2, s) = bbigStep (b1, s) || bbigStep (b2, s)
bbigStep (Leq e1 e2, s) = ebigStep (e1, s) <= ebigStep (e2, s)
bbigStep (Igual e1 e2, s) = ebigStep (e1, s) == ebigStep (e2, s)




-- A função `cbigStep` avalia comandos imperativos no contexto de uma memória dada.
-- Ela recebe um par (C, Memoria), onde `C` representa o comando e `Memoria` é o estado atual das variáveis.
cbigStep :: (C, Memoria) -> (C, Memoria)

-- Caso base: a expressão é `Skip`.
cbigStep (Skip, s) = (Skip, s)

-- Caso da expressão `If b c1 c2`:
cbigStep (If b c1 c2, s)
  | bbigStep (b, s) = cbigStep (c1, s)
  | otherwise = cbigStep (c2, s)

-- Caso da expressão `Seq c1 c2`:
cbigStep (Seq c1 c2, s) = let (_, s1) = cbigStep (c1, s)
                          in cbigStep (c2, s1)

-- Caso da expressão `Atrib (Var x) e`:
cbigStep (Atrib (Var x) e, s) = (Skip, mudaVar s x (ebigStep (e, s)))

-- Caso da expressão `While b c`:
cbigStep (While b c, s)
  | bbigStep (b, s) = let (_, s1) = cbigStep (c, s)
                      in cbigStep (While b c, s1)
  | otherwise = (Skip, s)

-- Caso da expressão `DoWhile c b`:
cbigStep (DoWhile c b, s) = let (_, s1) = cbigStep (c, s)
                            in if bbigStep (b, s1)
                               then cbigStep (DoWhile c b, s1)
                               else (Skip, s1)
-- Caso da expressão `loop`:
cbigStep (Loop (Num n) c, s)
  | n > 0     = let (_, s1) = cbigStep (c, s)
                in cbigStep (Loop (Num (n - 1)) c, s1)
  | otherwise = (Skip, s)

-- Caso da expressão `DAtrrib e1 e2 e3 e4`:
cbigStep (DAtrrib (Var x1) (Var x2) e1 e2, s) =
  let val1 = ebigStep (e1, s)
      val2 = ebigStep (e2, s)
      s1 = mudaVar s x1 val1
  in (Skip, mudaVar s1 x2 val2)
  
-- Caso da expressão Unless b c1 c2:
cbigStep (Unless b c1 c2, s)
  | bbigStep (b, s) = cbigStep (c2, s)
  | otherwise = cbigStep (c1, s)

--------------------------------------
---
--- Exemplos de programas para teste
---
--- O ALUNO DEVE IMPLEMENTAR EXEMPLOS DE PROGRAMAS QUE USEM:
--- * Loop 
-- cbigStep (loopExample, exSigma)
loopExample1 :: C
loopExample1 = Loop (Num 3) (Atrib (Var "y") (Soma (Var "y") (Num 1)))
--- * Dupla Atribuição
-- cbigStep (dualAssignExample, memoriaInicial)
dualAssignExample1 :: C
dualAssignExample1 = DAtrrib (Var "x") (Var "y") (Soma (Var "x") (Num 1)) (Sub (Var "y") (Num 1))
--- * Do While
-- cbigStep (doWhileExample1, memoriaInicial)
doWhileExample1 :: C
doWhileExample1 = DoWhile (Atrib (Var "y") (Soma (Var "y") (Num 1))) (Leq (Var "y") (Num 5))
-------------------------------------

---
--- O progExp1 é um programa que usa apenas a semântica das expressões aritméticas. Esse
--- programa já é possível rodar com a implementação inicial  fornecida:
---
--- para rodar:
-- *Main> ebigStep (progExp1, exSigma)
-- 13
-- *Main> ebigStep (progExp1, exSigma2)
-- 6
progExp1 :: E
progExp1 = Soma (Num 3) (Soma (Var "x") (Var "y"))

--- Para rodar os próximos programas é necessário primeiro implementar as regras da semântica
---

-- Subtração
-- *Main> ebigStep (progExp2, exSigma)
progExp2 :: E
progExp2 = Sub (Soma (Var "x") (Var "y")) (Num 2)

-- Multipicação
-- *Main> ebigStep (progExp3, exSigma)
-- *Main> ebigStep (progExp3, exSigma2)
progExp3 :: E
progExp3 = Mult (Var "x") (Var "y")

-- Divisão
-- *Main> ebigStep (progExp4, exSigma) 
-- *Main> ebigStep (progExp4, exSigma2)
progExp4 :: E
progExp4 = Div (Var "x") (Var "y")


---
--- Exemplos de expressões booleanas:
-- *Main> bbigStep (teste1, exSigma)
-- Comparação de menor ou igual com valores constantes.
teste1 :: B
teste1 = (Leq (Soma (Num 3) (Num 3))  (Mult (Num 2) (Num 3)))

-- *Main> bbigStep (teste2, exSigma)
-- Comparação de menor ou igual com variável "x".
teste2 :: B
teste2 = (Leq (Soma (Var "x") (Num 3))  (Mult (Num 2) (Num 3)))

-- Negação
-- *Main> bbigStep (teste3, exSigma)
teste3 :: B
teste3 = Not (Leq (Num 5) (Num 3))

-- And
--  *Main> bbigStep (teste4, exSigma)
teste4 :: B
teste4 = And (Leq (Num 2) (Num 5)) (Igual (Var "x") (Num 10))

--  OR
-- *Main> bbigStep (teste5, exSigma)
teste5 :: B
teste5 = Or (Leq (Num 4) (Num 5)) (Igual (Var "y") (Num 1))

-- Igualdade
-- *Main> bbigStep (teste6, exSigma)
teste6 :: B
teste6 = Igual (Soma (Num 3) (Num 2)) (Mult (Num 1) (Num 5))


-- comandos imperativos para a função cbigStep
-- Exemplos de Programas Imperativos:

-- While
-- *Main> cbigStep (whileExample, exSigma)
whileExample :: C
whileExample = While (Leq (Var "x") (Num 5))
                      (Atrib (Var "x") (Soma (Var "x") (Num 1)))

-- IF
-- *Main> cbigStep (ifExample, exSigma)
ifExample :: C
ifExample = If (Leq (Var "x") (Num 5))
               (Atrib (Var "y") (Num 1))
               (Atrib (Var "y") (Num 2))
-- SEQ
-- Executa dois comandos sequencialmente.
-- *Main> cbigStep (seqExample, exSigma)
seqExample :: C
seqExample = Seq (Atrib (Var "x") (Num 10))
                 (Atrib (Var "y") (Soma (Var "x") (Num 5)))

-- DoWhile
-- Executa um comando e, em seguida, verifica a condição. Repete o comando enquanto a condição for verdadeira.
-- *Main> cbigStep (doWhileExample, exSigma)
doWhileExample :: C
doWhileExample = DoWhile (Atrib (Var "x") (Soma (Var "x") (Num 1)))
                         (Leq (Var "x") (Num 5))

-- Loop
-- Executa um comando um número específico de vezes.
-- *Main> cbigStep (loopExample, exSigma)
loopExample :: C
loopExample = Loop (Num 3)
                   (Atrib (Var "x") (Soma (Var "x") (Num 1)))

-- DAtrrib
-- Atribui valores a duas variáveis a partir de duas expressões diferentes.
-- *Main> cbigStep (dualAssignExample, exSigma)
dualAssignExample :: C
dualAssignExample = DAtrrib (Var "x") (Var "y")
                            (Soma (Num 1) (Num 2))
                            (Sub (Num 5) (Num 3))
                            
-- Unless
-- Executa o primeiro comando se a condição for falsa, caso contrário, executa o segundo comando.
-- *Main> cbigStep (unlessExample, exSigma)
unlessExample :: C
unlessExample = Unless (Leq (Var "x") (Num 5))
                       (Atrib (Var "y") (Num 1))
                       (Atrib (Var "y") (Num 2))
                  
-- Swap
-- Troca os valores de duas variáveis.
-- *Main> cbigStep (swapExample, exSigma)
swapExample :: C
swapExample = Swap (Var "x") (Var "y")


-- Executa uma sequência de atribuições onde z recebe o valor de x, 
-- x recebe o valor de y, e finalmente y recebe o valor de z.
-- *Main> cbigStep (testec1, exSigma)
testec1 :: C
testec1 = (Seq (Seq (Atrib (Var "z") (Var "x")) (Atrib (Var "x") (Var "y"))) 
               (Atrib (Var "y") (Var "z")))

-- Calcula o fatorial de um número armazenado em x. 
-- O resultado é armazenado em y.
-- *Main> cbigStep (fatorial, exSigma)
fatorial :: C
fatorial = (Seq (Atrib (Var "y") (Num 1))
                (While (Not (Igual (Var "x") (Num 1)))
                       (Seq (Atrib (Var "y") (Mult (Var "y") (Var "x")))
                            (Atrib (Var "x") (Sub (Var "x") (Num 1))))))

-- Incrementa x em 1 se x for menor ou igual a 10.
-- *Main> cbigStep (conditionalIncrement, exSigma)
conditionalIncrement :: C
conditionalIncrement = If (Leq (Var "x") (Num 10))
                          (Atrib (Var "x") (Soma (Var "x") (Num 1)))
                          Skip

-- Decrementa o valor da variável counter 
-- até que ela seja menor ou igual a 0.        
-- *Main> cbigStep (decrementExample, exSigma)                  
decrementExample :: C
decrementExample = DoWhile (Atrib (Var "counter") (Sub (Var "counter") (Num 1))) (Leq (Var "counter") (Num 0))

-- Atribui o menor valor entre a e b para a variável min.
-- *Main> cbigStep (minExample, exSigma3)
minExample :: C
minExample = If (Leq (Var "a") (Var "b"))
                (Atrib (Var "min") (Var "a"))
                (Atrib (Var "min") (Var "b"))




     
