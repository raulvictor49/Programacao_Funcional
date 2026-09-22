-- I. Fatores primos (Nível: Difícil)
-- Passados os argumentos 5 [2,5,5] retorna quantas vezes o 5 apareceu
contaElemento :: Int -> [Int] -> Int -- 5 [2,5,5] = 2
contaElemento n [] = 0
contaElemento n (x:xs) | n == x = 1 + contaElemento n xs
                       | otherwise = contaElemento n xs

-- Retorna a lista de fatores primos do valor n, o divisor sempre começa com o valor 2
fatoracao :: Int -> Int -> [Int] -- 50 2 = [2,5,5]
fatoracao n divisor | n < 2 = []
                    | divisor * divisor > n = [n]
                    | n >= 2 && (mod n divisor) == 0 = divisor : fatoracao (n `div` divisor) divisor
                    | otherwise = fatoracao n (divisor+1)

contaElementosLista :: [Int] -> [Int]
contaElementosLista lista =
    auxiliar (listaElementoUnico lista) lista
    where
        auxiliar [] listaOriginal = []
        auxiliar (x:xs) listaOriginal =
            contaElemento x listaOriginal :
            auxiliar xs listaOriginal

-- Recebe uma lista e remove os elementos repetidos
-- listaElementoUnico :: [Int] -> [Int] -- [2,5,5] = [2,5]
-- listaElementoUnico lista = nub (lista) -- import Data.List

listaElementoUnico :: [Int] -> [Int]
listaElementoUnico [] = []
listaElementoUnico (x:xs)
    | pertence x xs = listaElementoUnico xs
    | otherwise = x : listaElementoUnico xs

pertence :: Int -> [Int] -> Bool
pertence n [] = False
pertence n (x:xs)
    | n == x    = True
    | otherwise = pertence n xs

fatPrime :: Int -> [(Int, Int)]
fatPrime n =
    zip fatoresUnicos (contaElementosLista fatores)
    where
        fatores = fatoracao n 2
        fatoresUnicos = listaElementoUnico fatores
