data Fracao = Fracao Int Int 
    deriving (Read, Show)
-- Fracao p q representa p/q, com 0 < p < q e MDC(p,q) = 1

fracaoN :: Int -> Int -> [Int]
fracaoN p q 
  | p == 1    = [q]
  | otherwise = n : fracaoN pSimples qSimples
  where 
    n        = ceiling (fromIntegral q / fromIntegral p)
    p2       = p * n - q
    q2       = q * n
    g        = gcd p2 q2          -- Calcula o MDC entre o resto
    pSimples = p2 `div` g         -- Simplifica o numerador
    qSimples = q2 `div` g         -- Simplifica o denominador

fracaoEgipcia :: Fracao -> [Int]
fracaoEgipcia (Fracao p q) = fracaoN p q