data Tipo = Fogo | Agua | Planta | Eletrico | Psiquico
    deriving (Eq, Read, Show)

bonusTipo :: Tipo -> Int
bonusTipo Fogo     = 10
bonusTipo Agua     = 8
bonusTipo Planta   = 7
bonusTipo Eletrico = 9
bonusTipo Psiquico = 12

data Pokemon = Pokemon String Tipo Int Int
    deriving (Read, Show)

pontuacaoTotal :: Int -> Int -> Tipo -> Int
pontuacaoTotal hp atq bonusT = hp + atq + bonusTipo bonusT

classificacao :: Int -> String
classificacao pontuacaoT
    | pontuacaoT >= 180                     = "Lendario"
    | pontuacaoT >= 120 && pontuacaoT < 180 = "Forte"
    | otherwise                             = "Normal"

processaPokemons :: [Pokemon] -> [(String, Int, String)]
processaPokemons = map (\(Pokemon nome tipo hp atq) -> 
    let pts = pontuacaoTotal hp atq tipo
    in (nome, pts, classificacao pts))

-- processaPokemons :: [Pokemon] -> [(String, Int, String)]
-- processaPokemons = map (\(Pokemon nome tipo hp atq) -> 
--     (nome, pontuacaoTotal hp atq tipo, classificacao (pontuacaoTotal hp atq tipo))) 
