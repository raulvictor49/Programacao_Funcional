{-
1 ) (2.5) Defina uma função que insira uma lista (primeiro parâmetro) em uma outra lista (segundo parâmetro) em uma posição informada (terceiro parâmetro). 
Se a posição por menor ou igual a zero insira a primeira lista no início da segunda lista;
se for igual ou maior que o tamanho da segunda lista, insira ela no final.

insert :: [t] -> [t] -> Int -> [t]

exemplo: insert "abc" "xyz 123 def" 4 --> "xyz abc123 def"
exemplo: insert "abc" "xyz 123 def" 0 --> "abcxyz 123 def"
exemplo: inesrt [1,2] [3,4,5,6] 10 --> [3,4,5,6,1,2]
-}

insert :: [t] -> [t] -> Int -> [t]
insert [] [] _ = []
insert [] lista2 _ = lista2
insert lista1 [] _ = lista1
insert lista1 lista2 p | p <= 0 = lista1 ++ lista2
                       | p >= length lista2 = lista2 ++ lista1
                       | otherwise = [x | x <- take p lista2] ++ lista1 ++ [y | y <- drop p lista2]