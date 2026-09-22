-- G. Mova à direita! (Nível: Fácil)
addEspacos :: Int -> String
addEspacos n | n == 0 = ""
             | otherwise = " " ++ addEspacos (n-1)

paraDireita :: Int -> String -> String
paraDireita n palavra = addEspacos n ++ palavra