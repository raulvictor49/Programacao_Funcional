-- C. Ranking do time (Nível: Difícil)
type LeagueOfLegends = String
exemplo :: LeagueOfLegends
exemplo = "blue;Garen;12;3;8;blue;Lux;5;2;15;red;Darius;7;9;4;red;Jinx;18;2;10;"

tuplas5 :: String -> [(String, String, Int, Int, Int)]
tuplas5 str = agrupa (quebra str)
    where
        quebra [] = []
        quebra s =
            let (parte, resto) = span (/= ';') s
            in parte : quebra (drop 1 resto)

        agrupa [] = []
        agrupa (a:b:c:d:e:xs) = (a,b,read c,read d, read e) : agrupa xs -- read transforma String em Int

rankTime :: String -> String -> [(String, Double)]
rankTime team dados =
    qs [(campeao, calculaKDA k m a)
       | (time, campeao, k, m, a) <- tuplas5 dados
       , time == team]
    where
        calculaKDA k m a
            | m == 0    = fromIntegral (k + a)
            | otherwise = fromIntegral (round (kda * 100)) / 100
                where kda = fromIntegral (k + a) / fromIntegral m

-- Para ordenar apenas pelo segundo parâmetro
qs [] = []
qs ((x,y):xs) =
    qs (filter (\(a,b) -> b > y) xs) ++
    [(x,y)] ++
    qs (filter (\(a,b) -> b <= y) xs)