-- A. Ordena alunos (Nível: Média)
ordenaAlunos :: [(String, Int)] -> [(String, Int)]
ordenaAlunos [] = []
ordenaAlunos (x:xs) = ordenaAlunos (filter (<x) xs) ++ [x] ++ ordenaAlunos (filter (>x) xs)

-- Livro (ordena uma lista)
-- qs [] = []
-- qs (x:xs) = qs (filter (<x) xs) ++ [x] ++ qs (filter (>x) xs)

-- Para ordenar apenas pelo primeiro parâmetro
-- qs [] = []
-- qs ((x,y):xs) =
--     qs (filter (\(a,b) -> a < x) xs) ++
--     [(x,y)] ++
--     qs (filter (\(a,b) -> a > x) xs)