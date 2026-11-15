-- funções sugeridas
-- putStrLn, ord (data.char), chr (data.char)
-- funcoes: succ, toLower, isAlpha

-- ord: Recebe um caractere (Char) e retorna o seu código numérico Unicode/ASCII (Int). Exemplo: ord 'A' -> 65
-- chr: Recebe um código numérico (Int) e retorna o caractere (Char) correspondente. Exemplo: chr 65 -> 'A'

-- Etapas:
-- 1: verificar se o caractere é char ou int
-- 2: verificar se o caractere é maiúscula ou minúscula
-- 3: utilizar a função succ x vezes (iter)


import Data.Char

deslocar :: Int -> Char -> Char
deslocar n c | isAlpha c = chr (ord 'a' + (ord (toLower c) - ord 'a' + n) `mod` 26)
             | otherwise = c

cifraCesar :: Int -> String -> String
cifraCesar _ [] = []
cifraCesar n (x:xs) = deslocar n x : cifraCesar n xs
