{-
4) (2.5) Dado o tipo de dados abaixo, que indica se um valor é válido ou inválido (incorreto) defina uma função em que seja passado um valor de hora e minutos em formato de 24h e a função converta pra o formato TimeOfDay, se for possível, 
retornando o resultado com o construtor Success e a hora no novo formato, ou retornando Fail (falha/erro) se o formato for inválido (não for menor que 24 ou os mintutos não forem menores que 60).

data Resultado t = Success t | Fail
                    deriving Show

convertTime :: Int -> Int -> Resultado TimeOfDay

exemplo: converter a hora 22:10
convertTime 22 10 ---> Success (PM 10 10)
convertTime 24 62 ---> Fail
-}

data Resultado t = Success t | Fail
                    deriving Show

data TimeOfDay = AM Int Int -- hora e minuto antes do meio dia
               | PM Int Int -- hora e minuto após o meio dia
                    deriving (Show)

convertTime :: Int -> Int -> Resultado TimeOfDay
convertTime hora minuto | hora < 0 || hora > 23 = Fail
                        | minuto < 0 || minuto > 59 = Fail
                        | hora > 11 = Success (PM (mod hora 12) minuto)
                        | otherwise = Success (AM hora minuto)