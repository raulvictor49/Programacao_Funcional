-- B. Triplas pitagóricas (Nível: Média)
-- import Debug.Trace
quadradoPerfeito :: Int -> Bool
quadradoPerfeito n =
    let raiz = floor (sqrt (fromIntegral n))
    in raiz * raiz == n

auxiliar :: Int -> Int -> Int -> [(Int, Int, Int)]
auxiliar n a b | a > (n-2) = []
auxiliar n a b | (b < n) && (a < b) && (c <= n) && (quadradoPerfeito (a^2 + b^2)) = [(a, b, c)] ++ auxiliar n a (b+1)
               | (b < n) && (a < n) = auxiliar n a (b+1)
               | otherwise = auxiliar n (a+1) 1
               where c = floor (sqrt (fromIntegral (a^2 + b^2)))

pitagoras :: Int -> [(Int, Int, Int)]
pitagoras n = auxiliar n 1 1

{-
-- Para debug
auxiliar n a b
    | (b < n) && (a < b) && (quadradoPerfeito (a^2 + b^2)) =
        trace ("a = " ++ show a ++ ", b = " ++ show b) $
        [(a, b, c)] ++ auxiliar n a (b+1)

    | (b < n) && (a < n) =
        trace ("a = " ++ show a ++ ", b = " ++ show b) $
        auxiliar n a (b+1)

    | otherwise =
        trace ("a = " ++ show a ++ ", b = " ++ show b) $
        auxiliar n (a+1) 1

    where
        c = floor (sqrt (fromIntegral (a^2 + b^2)))
-}