-- F. Calculadora (Nível: Média)
type Comando = String
type Valor = Int

processa :: [(Comando, Valor)] -> Int -> Int
processa [] valor = valor

processa (x:xs) valor | fst(x) == "Soma"       = processa xs (valor + snd(x))
                      | fst(x) == "Subtrai"    = processa xs (valor - snd(x))
                      | fst(x) == "Multiplica" = processa xs (valor * snd(x))
                      | fst(x) == "Divide" && snd(x) == 0 = -666
                      | otherwise              = processa xs (valor `div` snd(x))

executa :: [(Comando, Valor)] -> Int
executa comandos = processa comandos 0