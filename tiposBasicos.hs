addEspacos :: Int -> String
addEspacos inteiro | inteiro == 0 = ""
                   | otherwise = " " ++ addEspacos (inteiro - 1)


paraDireita :: Int -> String -> String
paraDireita i s | i <= 0 = s
                | otherwise = addEspacos i ++ s


vendas :: Int -> Int
vendas 0 = 12
vendas 1 = 14
vendas 2 = 15
vendas 3 = 12
vendas 4 = 88
vendas 5 = 33
vendas n = 0

cabecalho :: String
cabecalho = "semana     venda\n"

imprimeSemanas :: Int -> String
imprimeSemanas i | i == 5 = addEspacos 3 ++ show(5) ++ addEspacos 8 ++ show(vendas 5) ++ "\n"
                 | otherwise = addEspacos 3 ++ show(i) ++ addEspacos 8 ++ show(vendas i) ++"\n" ++ imprimeSemanas (i + 1)

totalVendas :: Int -> Int
totalVendas 0 = vendas 0
totalVendas n = vendas n + totalVendas (n-1)

imprimeTotal :: Int -> String
imprimeTotal n = "Total" ++ addEspacos 7 ++ show(totalVendas n) ++ "\n"

mediaVendas :: Int -> Int
mediaVendas 0 = vendas 0
mediaVendas n = div (totalVendas n) n

imprimeMedia :: Int -> String
imprimeMedia n = "Média" ++ addEspacos 7 ++ show(mediaVendas n)

imprimeTabela :: Int -> IO()
imprimeTabela n = putStr (cabecalho ++ imprimeSemanas (n - n) ++ imprimeTotal n ++ imprimeMedia n)