-- E. Fatura do cartão (Nível: Difícil)
type StringFatura = String
input :: StringFatura
input = "14 JAN;Amazon;40.32;15 JAN;Uber;14.84;25 JAN;Uber;34.24;02 FEV;Spotify;8.50;06 FEV;Uber;6.94;05 MAR;Burger;29.90;10 MAR;Burger;24.99;15 MAR;UCI;19.00;08 ABR;Itunes;3.50;13 ABR;Picpay;20.00;"

listaConcatenada :: String -> [String]
listaConcatenada str = quebra str
    where
        quebra [] = []
        quebra s =
            let (parte, resto) = span (/= ';') s
            in parte : quebra (drop 1 resto)


tuplas4 :: String -> [(Int, String, String, Double)]
tuplas4 str = transforma (listaConcatenada str)
    where
        transforma [] = []
        transforma (diaMes:empresa:valor:xs) =
            agrupa diaMes empresa valor : transforma xs

        agrupa diaMes empresa valor =
            let (dia, mes) = separaDiaMes diaMes
            in (read dia, mes, empresa, read valor)

        separaDiaMes s =
            let (dia, mes) = span (/= ' ') s
            in (dia, drop 1 mes)


valorFatura :: String -> [(Int, String, String, Double)] -> Double
valorFatura mes faturaAnual = sum (map (\(dia, mesFat, empresa, valor) -> valor)
                                  (filter (\(dia, mesFat, empresa, valor) -> mes == mesFat) faturaAnual))

logMes :: String -> String -> Double
logMes mes fatura = valorFatura mes (tuplas4 fatura)