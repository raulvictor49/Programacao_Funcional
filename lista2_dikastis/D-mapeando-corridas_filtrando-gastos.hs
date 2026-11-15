-- Etapas:
-- 1: verificar se o app alvo é igual a primeira entrada e a avaliação é >= 4 (OK)
-- 2: se km < 2.0 e nota == 5 = corrida gratuita
-- 3: se km >= 2.0 ou nota < 5 = corrida será km * 2.5
-- 4: se for horário de pico = corrida + 5.0

data Viagem = Viagem String Float Int Bool
    deriving (Show, Eq, Read) 

exemploEntrada :: [Viagem]
exemploEntrada = [Viagem "Uber" 1.5 5 False, Viagem "Uber" 10.0 5 True, Viagem "99Pop" 5.0 4 False, Viagem "Uber" 4.0 3 False]

filtroAppENota :: String -> [Viagem] -> [Viagem]
filtroAppENota appAlvo = filter (\(Viagem app _ nota _) -> appAlvo == app && nota >= 4)

tarifa :: Float -> Int -> Bool -> Float
tarifa dist nota pico | dist < 2.0 && nota == 5               = 0.0
                      | (dist >= 2.0 || nota < 5) && not pico = dist * 2.5
                      | (dist >= 2.0 || nota < 5) && pico     = dist * 2.5 + 5.0
                      | otherwise                             = 0.0

calculaTodasTarifas :: [Viagem] -> Float
calculaTodasTarifas = sum . map (\(Viagem _ dist nota pico) -> tarifa dist nota pico)

custoTotal :: String -> [Viagem] -> Float
custoTotal appAlvo = calculaTodasTarifas . filtroAppENota appAlvo