data Nacao = Agua | Terra | Fogo | Ar 
            deriving (Read, Show, Eq)

data Ataque = Simples Nacao Int
            | Combinado Nacao Nacao Int
            | EstadoAvatar Int
            deriving (Read, Show)

fraqueza :: Nacao -> Nacao
fraqueza Fogo  = Agua
fraqueza Agua  = Terra
fraqueza Terra = Ar
fraqueza Ar    = Fogo

danoTotal :: Nacao -> [Ataque] -> Int
danoTotal def atq = calcularDano def atq False

calcularDano :: Nacao -> [Ataque] -> Bool -> Int
calcularDano _ [] _ = 0
calcularDano def (atk:atks) avatarAtivo = case atk of
    
    Simples n d 
        | n == fraqueza def -> (d * 2 * multAvatar) + calcularDano def atks False
        | otherwise         -> (d * multAvatar)     + calcularDano def atks False
        
    Combinado n1 n2 d 
        | n1 == fraqueza def || n2 == fraqueza def -> (d * 2 * multAvatar) + calcularDano def atks False
        | otherwise                                -> (d * multAvatar)     + calcularDano def atks False
        
    EstadoAvatar d -> (d * 3 * multAvatar) + calcularDano def atks True

  where
    -- Multiplicador do Estado Avatar atual
    multAvatar 
        | avatarAtivo = 2
        | otherwise   = 1