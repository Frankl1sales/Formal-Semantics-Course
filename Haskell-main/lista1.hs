----------------  Funamentos de Haskell ---------------------- 
--------------------------------------------------------------
--------------------------------------------------------------
--------------------------------------------------------------
---------------- lista de exercícios 1 -----------------------
--------------------------------------------------------------
--------------------------------------------------------------
-- (1)Função que verifica se uma string contém um palíndromo
palindromo :: String -> Bool
palindromo str = str == reverse str

-- (2) verifica triangulo
verificaTriangulo :: Int -> Int -> Int -> Bool
verificaTriangulo a b c = (a+b>c)&&(a+c >b)&&(b+c > a)

-- exemplo que professor pediu
uns :: [Int]
uns = 1 : uns

-- (3)
sinal :: Int -> Int
sinal a  
 | a > 0   = 1
 | a == 0  = 0
 | otherwise = -1

-- (4)
menorTres :: Int -> Int -> Int -> Int
menorTres a b c
 | (a < b)&&(a < c) = a
 | (b < a)&&(b < c) = b
 | otherwise = c

-- (5)
-- Função recursiva para calcular potência
potencia :: Int -> Int -> Int
potencia base 0 = 1   -- Qualquer número elevado a 0 é 1
potencia base exp = base * potencia base (exp - 1)
--------------------------------------------------------------
--------------------------------------------------------------
--------------------------------------------------------------
-------------------------------------------------------------
------ Lista de exercícios extra de Haskell-------------------
--------------------------------------------------------------
--------------------------------------------------------------
--------------------------------------------------------------
--------------------------------------------------------------
--------------------------------------------------------------
-- (1)
osQuatroSaoIguais :: Int -> Int -> Int -> Int -> Bool
osQuatroSaoIguais a b c d = (a==b)&&(b==c)&&(c==d)

-- (2)
quantosSaoIguais :: Int -> Int -> Int -> Int
quantosSaoIguais a b c
 | (a==b)&&(a==c) = 3
 | (a==b)||(a==c)||(b==c) = 2
 | otherwise = 0

-- (3)
todosDiferentes :: Int -> Int -> Int -> Bool
todosDiferentes a b c = (a /= b) &&(a/=c)&&(b/=c)

-- (4)
 -- todosDiferentes n m p = ( ( n/=m ) && ( m/=p ) )
 -- A expressão não verifica se 'n' é diferente de 'p'

-- (5)
todosIguais :: Int -> Int -> Int -> Bool
todosIguais n m p = (n == m) && (m == p)

quantosSaoIguais1 :: Int -> Int -> Int -> Int
quantosSaoIguais1 n m p
    | todosIguais n m p = 3  -- Se todos forem iguais, retorna 3
    | todosDiferentes n m p = 0  -- Se todos forem diferentes, retorna 0
    | otherwise = 2  -- Caso contrário, exatamente dois são iguais

-- (6)
elevadoDois :: Int -> Int
elevadoDois n = n ^ 2

-- (7)
elevadoQuatro :: Int -> Int
elevadoQuatro n = elevadoDois (elevadoDois n)

-- (8)
vendas :: Int -> Int
-- Exemplo fictício de implementação da função vendas
vendas semana
    | semana == 0 = 100  -- Exemplo de vendas na semana 0
    | semana == 1 = 150  -- Exemplo de vendas na semana 1
    | semana == 2 = 200  -- Exemplo de vendas na semana 2
    | otherwise = 0      -- Para semanas além de 0, 1 e 2 (fictício)

vendaTotal :: Int -> Int
vendaTotal 0 = vendas 0  -- Caso base: retorna vendas da semana 0
vendaTotal n = vendas n + vendaTotal (n - 1)  -- Caso recursivo: soma vendas da semana n com vendaTotal das semanas anteriores
--------------------------------------------------------------
--------------------------------------------------------------
--------------------------------------------------------------
--------------------------------------------------------------
--------------- Fundamentos: LISTAS --------------------------
--------------------------------------------------------------
--------------------------------------------------------------
--------------------------------------------------------------
--------------------------------------------------------------
--------------------------------------------------------------
-- Exemplo do Slide 
somaLista :: [Int] -> Int
somaLista [] = 0
somaLista (x:xs) = x + somaLista xs

-- Exemplo do Slide 
multDois:: [Int] -> [Int]
multDois [] = []
multDois (x:xs) = 2*x : multDois xs
--------------------------------------------------------------
--------------------------------------------------------------
--------------------------------------------------------------
--------------------------------------------------------------
---------- Lista de exercícios 2 de Haskell-------------------
--------------------------------------------------------------
--------------------------------------------------------------
--------------------------------------------------------------
--------------------------------------------------------------
--------------------------------------------------------------
--(1)
multLista :: Int -> [Int] -> [Int]
multLista a [] = []
multLista a (x:xs) = a*x: multLista a xs

-- (2)
--elemento :: Int -> [Int] -> Bool
elemento :: Int -> [Int] -> Bool
elemento _ [] = False
elemento a (x:xs)
    | a == x    = True
    | otherwise = elemento a xs

-- (3)
conta :: Int -> [Int] -> Int
conta _ [] = 0
conta a (x:xs)
    | a == x = 1 + conta a xs
    | otherwise = conta a xs

-- (4)
contaMaiores :: Int -> [Int]-> Int
contaMaiores _ [] = 0
contaMaiores a (x:xs)
    | a < x = 1 +contaMaiores a xs
    | otherwise = contaMaiores a xs

-- (5)
maiores :: Int -> [Int] -> [Int]
maiores _ [] = [] 
maiores a (x:xs) 
    | a < x = x : maiores a xs 
    | otherwise = maiores a xs

-- (6)
geraLista :: Int -> Int -> [Int]
geraLista 0 _ = []
geraLista m n = n :geraLista (m-1) n

-- (7)
addFim :: Int -> [Int] -> [Int]
addFim a [] = [a]
addFim a (x:xs) = x : addFim a xs

-- (8)
-- Função para concatenar duas listas
join :: [Int] -> [Int] -> [Int]
join [] ys = ys                  -- Se a primeira lista estiver vazia, retorna a segunda lista
join (x:xs) ys = x : join xs ys  -- Adiciona o primeiro elemento da primeira lista ao resultado da concatenação do restante da primeira lista com a segunda lista

-- (9)
-- Função para inverter uma lista
inverte :: [Int] -> [Int]
inverte [] = []                       -- Caso base: lista vazia
inverte (x:xs) = inverte xs ++ [x]    -- Caso geral: inverte o restante da lista e adiciona o elemento atual no final

-- (9-b)
-- Função auxiliar para acumular elementos na ordem invertida
inverteAux :: [Int] -> [Int] -> [Int]
inverteAux [] acc = acc               -- Caso base: lista vazia, retorna o acumulador
inverteAux (x:xs) acc = inverteAux xs (x:acc)  -- Adiciona o elemento atual no início do acumulador

-- Função principal para inverter uma lista
inverteB :: [Int] -> [Int]
inverteB lista = inverteAux lista []  -- Chama a função auxiliar com a lista original e um acumulador vazio
--------------------------------------------------------------
--------------------------------------------------------------
--------------------------------------------------------------
--------------------------------------------------------------
------ Lista de exercícios 2 extra de Haskell ----------------
--------------------------------------------------------------
--------------------------------------------------------------
--------------------------------------------------------------
--------------------------------------------------------------
--------------------------------------------------------------
-- Exemplo dos Slides
ins :: Int -> [Int] -> [Int]
ins a [] = [a]
ins a (x:xs)
    | a <= x = a:x:xs
    | otherwise = x: ins a xs

-- (1)
pegaPosi :: Int -> [Int] -> Int
pegaPosi _ [] = error "Índice fora dos limites da lista" -- Caso de lista vazia
pegaPosi 0 (x:_) = x -- Caso base: índice 0, retorna o primeiro elemento
pegaPosi n (_:xs) = pegaPosi (n-1) xs -- Caso recursivo: diminui o índice e continua na lista

-- (2)
--pega 3 [1,2,3,4,5]
-- [1,2,3]
pega :: Int -> [Int] -> [Int]
pega _ [] = [] -- Caso de lista vazia: retorna lista vazia
pega 0 _ = [] -- Caso base: n é 0, retorna lista vazia ou a lista mesmo
pega n (x:xs) = x : pega (n-1) xs -- Caso recursivo: pega o primeiro elemento e chama recursivamente

-- (3)
retira :: Int -> [Int] -> [Int]
retira _ [] = [] -- Caso de lista vazia: retorna lista vazia
retira 0 xs = xs -- Caso base: n é 0, retorna a lista original
retira n (_:xs) = retira (n-1) xs -- Caso recursivo: ignora o primeiro elemento e chama recursivamente

-- (4)
-- Implementar a função mediaLista que a ula a média dos elementos de uma lista
-- Função para calcular a soma dos elementos da lista
--Está na linha 109
somaListaFloat :: [Float] -> Float
somaListaFloat [] = 0
somaListaFloat (x:xs) = x + somaListaFloat(xs)

-- Função para contar o número de elementos na lista
contaElementos :: [Float] -> Int
contaElementos [] = 0
contaElementos (_:xs) = 1 + contaElementos(xs)

-- Função para calcular a média dos elementos da lista
mediaLista :: [Float] -> Float
mediaLista [] = error "Não há elementos na lista" -- Caso de lista vazia
mediaLista xs = (somaListaFloat xs) / fromIntegral (contaElementos xs)
-- `div` retorna o inteiro da divisão

-- (5)
-- pegaMaiores 3 [10,2,3,4,5]
-- [10,4,5]
pegaMaiores :: Int -> [Int] -> [Int]
pegaMaiores _ [] = [] -- Caso base: lista vazia
pegaMaiores a (x:xs)
    | x > a     = x : pegaMaiores a xs -- Se o elemento é maior que 'a', inclui na lista resultante
    | otherwise = pegaMaiores a xs     -- Caso contrário, ignora o elemento

-- (6)
--Implementar a função ontaMaiores
-- contaMaiores 3 [10,2,3,4,5,16]
--contaMaiores :: Int -> [Int] -> Int
--contaMaiores _ [] = 0 
--contaMaiores a (x:xs)
--    | x > a 
--    |
-- Função principal para testes
--------------------------------------------------------------
--------------------------------------------------------------
--------------------------------------------------------------
--------------------------------------------------------------
--------- Fundamentos: Tipo Algebricos -----------------------
--------------------------------------------------------------
--------------------------------------------------------------
--------------------------------------------------------------
--------------------------------------------------------------
--------------------------------------------------------------

-- Definição da árvore
data Arvore = Folha Int | Nodo Int Arvore Arvore
  deriving (Eq, Show)

-- Exemplo de árvore
arv1 :: Arvore
arv1 = Nodo 10 (Nodo 14 (Nodo 1 (Folha 4) (Folha 2)) (Folha 6)) (Folha 9)

-- (1) Função para multiplicar todos os valores na árvore por um inteiro
multArvore :: Int -> Arvore -> Arvore
multArvore x (Folha v) = Folha (x * v)
multArvore x (Nodo v esq dir) = Nodo (x * v) (multArvore x esq) (multArvore x dir)

-- (1) Aplicar multiplicação por 3
resultado :: Arvore
resultado = multArvore 3 arv1

-- (2) Função para contar o número de folhas na árvore
contaFolhas :: Arvore -> Int
contaFolhas (Folha _) = 1  -- Conta uma folha
contaFolhas (Nodo _ esq dir) = contaFolhas esq + contaFolhas dir  -- Soma as folhas das subárvores esquerda e direita

-- (3) Função para contar o número de nós (Nodo) na árvore
contaNodos :: Arvore -> Int
contaNodos (Folha _) = 0  -- Folhas não são contadas como nós
contaNodos (Nodo _ esq dir) = 1 + contaNodos esq + contaNodos dir -- Conta o nó atual e soma os nós das subárvores esquerda e direita

-- (4) Função para contar quantas vezes um inteiro aparece na árvore
quantasVezes :: Int -> Arvore -> Int
quantasVezes x (Folha v) = if x == v then 1 else 0
quantasVezes x (Nodo v esq dir) = 
    (if x == v then 1 else 0) + quantasVezes x esq + quantasVezes x dir

-- (5) Função para encontrar o maior valor em uma árvore
maxArvore :: Arvore -> Int
maxArvore (Folha v) = v
maxArvore (Nodo v esq dir) = max v (max (maxArvore esq) (maxArvore dir))


-- (6) Função para refletir uma árvore, trocando os ramos esquerdos e direitos
refleteArvore :: Arvore -> Arvore
refleteArvore (Folha v) = Folha v
refleteArvore (Nodo v esq dir) = Nodo v (refleteArvore dir) (refleteArvore esq)

-- (7) Função para transformar uma árvore em uma lista de inteiros (versão 2)
geraLista2 :: Arvore -> [Int]
geraLista2 (Folha v) = [v]
geraLista2 (Nodo v esq dir) = v : geraLista2 esq ++ geraLista2 dir

--------------------------------------------------------------
--------------------------------------------------------------
--------------------------------------------------------------
--------------------------------------------------------------
--------- Fundamentos: Tipo Algebricos -----------------------
--------------------------------------------------------------
--------------------------------------------------------------
--------------------------------------------------------------
--------------------------------------------------------------
--------------------------------------------------------------

--data N = Zero | Suc N
--  deriving(Eq,Show)



-- Definindo o tipo de dado Nat
data Nat = Zero | Suc Nat
    deriving (Eq, Show)
somaALG :: Nat -> Nat -> Nat
somaALG Zero n = n
somaALG (Suc n1) n2 = Suc (somaALG n1 n2)

-- (1)Implementação da função to_int
to_int :: Nat -> Int
to_int Zero     = 0
to_int (Suc n)  = 1 + to_int n

-- (2) Implementar a função to_nat que transforma um inteiro em um Nat:
to_nat :: Int -> Nat
to_nat n
    | n <= 0    = Zero
    | otherwise = Suc (to_nat (n - 1))

-- (3) Função mult que multiplica dois Nats
mult :: Nat -> Nat -> Nat
mult Zero _         = Zero  -- Qualquer coisa vezes Zero1 é Zero1
mult (Suc n) m = somaALG m (mult n m)  -- Suc1 n * m = m + (n * m)

-- (4) Função leq (verifica se o primeiro Nat é menor ou igual ao segundo
leq :: Nat -> Nat -> Bool
leq Zero _ = True          -- Zero é sempre menor ou igual a qualquer Nat
leq _ Zero = False         -- Qualquer Nat é maior que Zero
leq (Suc n1) (Suc n2) = leq n1 n2  -- Compara os sucessores dos Nats
-- leq (to_nat 5) (to_nat 10)

-- (5) Função sub que executa a subtração de dois naturais
sub :: Nat -> Nat -> Nat
sub Zero _ = Zero            -- Zero menos qualquer número é Zero
sub n Zero = n               -- Qualquer número menos Zero é o próprio número
sub (Suc n1) (Suc n2) = sub n1 n2  -- Subtrai sucessores e continua a subtração

-- (6)Função geq que verifica se o primeiro Nat é maior ou igual ao segundo
geq :: Nat -> Nat -> Bool
geq Zero _ = False           -- Zero não é maior ou igual a nenhum Nat positivo
geq _ Zero = True            -- Qualquer Nat é maior ou igual a Zero
geq (Suc n1) (Suc n2) = geq n1 n2  -- Compara os sucessores dos Nats

-- (7) Função divi que calcula a divisão de dois naturais
divi :: Nat -> Nat -> Nat
divi _ Zero = error "Divisão por zero"
divi Zero _ = Zero
divi n1 n2 
    | geq n1 n2 = Suc (divi (sub n1 n2) n2)
    | otherwise  = Zero







main :: IO ()
main = do
    putStrLn "Teste de Funções Haskell"
    putStrLn ("Palíndromo 'arara': " ++ show (palindromo "arara"))
    putStrLn ("Verifica Triângulo 3, 4, 5: " ++ show (verificaTriangulo 3 4 5))
    putStrLn ("Sinal de -10: " ++ show (sinal (-10)))
    putStrLn ("Menor entre 1, 2, 3: " ++ show (menorTres 1 2 3))
    putStrLn ("2^3: " ++ show (potencia 2 3))



