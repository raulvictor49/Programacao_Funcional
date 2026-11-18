{-
Na copa do mundo de futebol, os grupos contém 4 times, e avançam para a fase seguinte os que têm maior número de pontos, sendo 3 pontos por uma vitória, 1 por um empate, e zero por uma derrota.
Vamos representar os times e jogos com o tipo de dados e o sinônimo de tipos abaixo:
-}

type Time = String

type Jogo = (Time, Int, Int, Time)
-- exemplo: ("Brasil", 1, 0, "Haiti") —--> significa Brasil 1 x 0 Haiti

time1, time2 :: Jogo -> Time
time1 (t1,g1,g2,t2) = t1
time2 (t1,g1,g2,t2) = t2

gols1, gols2 :: Jogo -> Int
gols1 (t1,g1,g2,t2) = g1
gols2 (t1,g1,g2,t2) = g2

jogos1 :: [Jogo]
jogos1 = [("Haiti", 1, 3, "Escocia"), ("Brasil", 2, 0, "Marrocos"),
          ("Catar", 0, 2, "Suica"), ("Canada", 0, 0, "Bosnia"),
          ("Suica", 0, 1, "Bosnia"), ("Canada", 0, 0, "Catar"),
          ("Brasil", 1, 0, "Haiti"), ("Escocia",0, 1, "Marrocos"),
          ("Suica", 1, 0, "Canada"), ("Bosnia", 1, 1, "Catar"),
          ("Marrocos", 0, 0, "Haiti"), ("Escocia",1, 2, "Brasil")]

-- 1 - (2,0 Pontos) Defina uma função que, dado um time e uma lista de jogos, informe quantos gols aquele time fez.
-- [esta questão tem mais 0,5 ponto se usar map/filter ou compreensões de listas na solução]

gols :: Time -> [Jogo] -> Int
-- exemplo: gols "Brasil" jogos1 —--> resposta: 5 (2+1+2)
gols time [] = 0
{- gols time ((t1,g1,g2,t2):jogos) | time == t1 = g1 + gols time jogos
                                | time == t2 = g2 + gols time jogos
                                | otherwise = gols time jogos -}
{- gols time (jogo:jogos) | time == time1 jogo = gols1 jogo + gols time jogos
                          | time == time2 jogo = gols2 jogo + gols time jogos
                          | otherwise = gols time jogos -}
gols time jogos = foldr (+) 0 (map conta_gols jogos)
   where conta_gols :: Jogo -> Int
         conta_gols (t1,g1,g2,t2) | time == t1 = g1
                                  | time == t2 = g2
                                  | otherwise = 0