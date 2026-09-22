-- D. Ajuda com média (Nível: Média)
mediaNotas :: [Double] -> Double
mediaNotas [] = 0
mediaNotas (x:xs) = sum (x:xs) / fromIntegral (length (x:xs))

processaAlunos :: [(String, [Double])] -> [(String, Double, String)]
processaAlunos [] = []
processaAlunos ((x, ys):xs) | mediaNotas ys >= 7 = (x, mediaNotas ys, "Aprovado") : processaAlunos xs
                            | mediaNotas ys < 5 = (x, mediaNotas ys, "Reprovado") : processaAlunos xs
                            | otherwise = (x, mediaNotas ys, "Recuperacao") : processaAlunos xs