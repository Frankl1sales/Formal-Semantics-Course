
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


type Memoria = [(String,Int)]

exSigma :: Memoria
exSigma = [ ("x", 10), ("temp",0), ("y",0)]


--- A função procuraVar recebe uma memória, o nome de uma variável e retorna o conteúdo
--- dessa variável na memória. Exemplo:
---
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

cbigStep (Loop (Num n) c, s)
  | n > 0     = let (_, s1) = cbigStep (c, s)
                in cbigStep (Loop (Num (n - 1)) c, s1)
  | otherwise = (Skip, s)


--------------------------------------
---
--- Exemplos de programas para teste
---
--- O ALUNO DEVE IMPLEMENTAR EXEMPLOS DE PROGRAMAS QUE USEM:
--- * Loop 
loopExample :: C
loopExample = Loop (Num 3) (Atrib (Var "y") (Soma (Var "y") (Num 1)))
--- * Dupla Atribuição
dualAssignExample :: C
dualAssignExample = DAtrrib (Var "x") (Var "y") (Soma (Var "x") (Num 1)) (Sub (Var "y") (Num 1))
--- * Do While
doWhileExample :: C
doWhileExample = DoWhile (Atrib (Var "y") (Soma (Var "y") (Num 1))) (Leq (Var "y") (Num 5))
-------------------------------------

exSigma2 :: Memoria
exSigma2 = [("x",3), ("y",0), ("z",0)]


---
--- O progExp1 é um programa que usa apenas a semântica das expressões aritméticas. Esse
--- programa já é possível rodar com a implementação inicial  fornecida:

progExp1 :: E
progExp1 = Soma (Num 3) (Soma (Var "x") (Var "y"))

---
--- para rodar:
-- *Main> ebigStep (progExp1, exSigma)
-- 13
-- *Main> ebigStep (progExp1, exSigma2)
-- 6

--- Para rodar os próximos programas é necessário primeiro implementar as regras da semântica
---


---
--- Exemplos de expressões booleanas:
-- let initialMemory = [("x", 3), ("y", 2), ("z", 0), ("counter", 5), ("a", 10), ("b", 20)]

teste1 :: B
teste1 = (Leq (Soma (Num 3) (Num 3))  (Mult (Num 2) (Num 3)))

teste2 :: B
teste2 = (Leq (Soma (Var "x") (Num 3))  (Mult (Num 2) (Num 3)))



---
-- Exemplos de Programas Imperativos:

testec1 :: C
testec1 = (Seq (Seq (Atrib (Var "z") (Var "x")) (Atrib (Var "x") (Var "y"))) 
               (Atrib (Var "y") (Var "z")))

fatorial :: C
fatorial = (Seq (Atrib (Var "y") (Num 1))
                (While (Not (Igual (Var "x") (Num 1)))
                       (Seq (Atrib (Var "y") (Mult (Var "y") (Var "x")))
                            (Atrib (Var "x") (Sub (Var "x") (Num 1))))))
 
swapExample :: C
swapExample = Swap (Var "x") (Var "y")


conditionalIncrement :: C
conditionalIncrement = If (Leq (Var "x") (Num 10))
                          (Atrib (Var "x") (Soma (Var "x") (Num 1)))
                          Skip
                          
decrementExample :: C
decrementExample = DoWhile (Atrib (Var "counter") (Sub (Var "counter") (Num 1))) (Leq (Var "counter") (Num 0))

minExample :: C
minExample = If (Leq (Var "a") (Var "b"))
                (Atrib (Var "min") (Var "a"))
                (Atrib (Var "min") (Var "b"))




     
