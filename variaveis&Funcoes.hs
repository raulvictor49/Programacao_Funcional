vendas :: Int -> Int
vendas 0 = 12
vendas 1 = 14
vendas 2 = 15
vendas 3 = 12
vendas 4 = 88
vendas 5 = 33
vendas n = 0

totalVendas :: Int -> Int
totalVendas 0 = vendas 0
totalVendas n = vendas n + totalVendas (n-1)

totalSemanas :: Int -> Int -> Int
totalSemanas s 0
    | vendas 0 == s = 1
    | otherwise     = 0

totalSemanas s n
    | vendas n == s = 1 + totalSemanas s (n - 1)
    | otherwise     = totalSemanas s (n - 1)


primo :: Int -> Bool
primo n
    | n < 2     = False
    | n == 2    = True
    | otherwise = verificaDivisor n 2

verificaDivisor :: Int -> Int -> Bool
verificaDivisor n d
    | d >= n     = True
    | mod n d == 0 = False
    | otherwise  = verificaDivisor n (d + 1)

primosEntreSi :: Int -> Int -> Bool
primosEntreSi x y
    | x < 2 || y < 2 = False
    | x == y = False
    | otherwise = verificaDivisorDuplo x y 2

verificaDivisorDuplo :: Int -> Int -> Int -> Bool
verificaDivisorDuplo x y d
    | d >= min x y = True
    | mod x d == 0 && mod y d == 0 = False
    | otherwise = verificaDivisorDuplo x y (d + 1)

fatorial :: Int -> Int
fatorial x | x <= 1 = 1
           | otherwise = x * fatorial (x - 1)

allEqual :: Int -> Int -> Bool
allEqual x y = (x == y)

all4Equal :: Int -> Int -> Int -> Int -> Bool
-- all4Equal x y z w = (x == y) && (y == z) && (z == w)
all4Equal x y z w = (allEqual x y) && (allEqual y z) && (allEqual z w)


equalMais1 :: Int -> Int -> Int
equalMais1 x y | allEqual x y = 1
               | otherwise = 0

equalCount :: Int -> Int -> Int -> Int -> Int
equalCount x y z w = (equalMais1 x y) + (equalMais1 x z) +
                     (equalMais1 x w) + (equalMais1 y z) +
                     (equalMais1 y w) + (equalMais1 z w)