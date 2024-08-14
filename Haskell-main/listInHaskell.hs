somaLista :: [Int] -> Int
somaLista [] = 0
somaLista (x:xs) = x + somaLista xs


multDois:: [Int] -> [Int]
multDois [] = []
multDois (x:xs) = 2*x : multDois xs


-- Exercícios de Lista 1

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

