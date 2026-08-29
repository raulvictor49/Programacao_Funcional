import Data.Char

myList :: Int -> Int -> [Int]
myList inicio fim = [inicio..fim]


minhaLista :: Int -> Int -> [Int]
minhaLista inicio fim
    | inicio > fim = []
    | otherwise = inicio : minhaLista (inicio + 1) fim


doubleList :: [Int] -> [Int]
doubleList [] = []
doubleList (x:xs) = (x*2) : doubleList xs
-- doubleList (x:xs) = [(x*2)] ++ doubleList xs


-- member :: [Int] -> Int -> Bool
-- member [] valor = False
-- -- member [] _ = False
-- member [x:xs] valor | x == valor = True
--                     |otherwise = member xs valor


digits :: String -> String
digits [] = []
digits (ch:chs) | isDigit ch = ch : digits chs
                | otherwise = digits chs

sumPairs :: [(Int,Int)] -> [Int]
sumPairs [] = []
sumPairs ((x,y) : xys) = (x+y) : sumPairs xys
-- sumPairs (xy : xys) = (fst xy + snd xy) : sumPairs xys