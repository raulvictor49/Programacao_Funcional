-- Funções de alta ordem
quadrado :: Int -> Int
quadrado x = x ^ 2

isCrescent :: (Int -> Int) -> Int -> Bool
isCrescent _ 0 = True
isCrescent f n = (f (n - 1) < f n) && isCrescent f (n - 1)

-- map e fold
mapExercicio :: (t -> u) -> [t] -> [u]
mapExercicio f [] = []
mapExercicio f (x:xs) = f x : mapExercicio f xs

maiorQue :: Ord a => a -> a -> Bool
maiorQue x y = x > y
-- Ex: determinar filtrar os elementos de uma lista, que são maiores que 0
-- filter (\x -> maiorQue x 0) [5, 12, -4, 8, -1, 0, 2]
-- filter (`maiorQue` 0) [5, 12, -4, 8, -1, 0, 2]
-- filter (maiorQue 0) [5, 12, -4, 8, -1, 0, 2] A função fica menorQue, pois 0 vira o primeiro arg.
-- filter (not . (maiorQue 0)) [5, 12, -4, 8, -1, 0, 2]

