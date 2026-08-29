soma :: Int -> Int -> Int
soma x y = x + y

answer :: Int
answer = 42

greater :: Bool
greater = (answer > 71)

yes :: Bool
yes = True

yES = False

square :: Int -> Int
square x = x * x

allEqual :: Int -> Int -> Bool
allEqual x y = (x == y)

maxi :: Int -> Int -> Int
maxi n m | n >= m = n
         | otherwise = m

addD :: Int -> Int -> Int
addD a b = 2 * (a+b)

-- vendas :: Int -> Int
-- vendas n = mod n 17

-- totalVendas :: Int -> Int
-- totalVendas n | n == 0    = 0
--               | n > 0     = vendas n + totalVendas (n - 1)
--               | otherwise = 0


opcoesDiv :: Int              -- Tarefa 3.5
opcoesDiv = a `div` length xs
    where
        a = 10
        xs = [1,2,3,4,5]


quadradoDobro :: Int -> Int   -- Tarefa 3.6
quadradoDobro a = (a * a) ^ 2


dobroQuadrado :: Int -> Int   -- Tarefa 3.7
dobroQuadrado a = (a ^ 2) * 2


ladosTriangulo :: Int -> Int -> Int -> Bool -- Tarefa 3.8
ladosTriangulo a b c = (a + b) > c &&
                       (a + c) > b &&
                       (b + c) > a


forcaMola :: Int -> Int -> Int -- Tarefa 3.9
forcaMola k x = k * x


energiaPotencial :: Double -> Double -> Double  -- Tarefa 3.9
energiaPotencial k x = 0.5 * k * (x ^ 2)


custoEletrico :: Double -> Double -> Double     -- Tarefa 3.10
custoEletrico salario kw = ((1 / 5) * salario * kw) * 0.85