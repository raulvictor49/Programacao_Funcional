{-
3) (2.5) Dado o tipo de dados abaixo, que representa as horas do dia em formato de 12h pela manhã (AM) e 12h a partir de meio dia (PM), defina uma função em que sejam
adicionados minutos a uma hora do dia, considerando a possível mudança de turno. 
assuma que o formato da hora é válido e que serão somados no máximo 719 minutos.

data TimeOfDay = AM Int Int -- hora e minuto antes do meio dia
                | PM Int Int -- hora e minuto após o meio dia
                deriving (Show)

somaMinutos :: TimeOfDay -> Int -> TimeOfDay

exemplo: somar 30 minutos ás 11:59 da manhã:
exemplo: somaMinutos (AM 11 59) 30 --> PM 12 29
exemplo: somaMinutos (PM 10 28) 22 --> PM 10 50
-}

data TimeOfDay = AM Int Int -- hora e minuto antes do meio dia
               | PM Int Int -- hora e minuto após o meio dia
                    deriving (Show)

extraTime :: Int -> Int -> Int -> (Int, (Int, Int))
extraTime h m t = (totalTime, (newHour, newMinutes))
    where
        totalTime = h*60 + (m + t)
        newHour = totalTime `div` 60
        newMinutes = totalTime `mod` 60

somaMinutos :: TimeOfDay -> Int -> TimeOfDay
somaMinutos turno tempoExtra = case turno of
    (AM hora minuto) -> (if fst newTime > 720 then uncurry PM (snd newTime) else uncurry AM (snd newTime)) 
        where newTime = extraTime hora minuto tempoExtra
    (PM hora minuto) -> (if fst newTime > 720 then uncurry AM (snd newTime) else uncurry PM (snd newTime))
        where newTime = extraTime hora minuto tempoExtra