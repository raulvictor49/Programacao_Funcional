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


-- TUPLA
menorMaior :: Int -> Int -> Int -> (Int, Int)
menorMaior a b c = (minimum [a, b, c], maximum [a, b, c])

ordenaTripla :: Int -> Int -> Int -> (Int, Int, Int)
ordenaTripla a b c = (minimum[a,b,c], a+b+c - minimum[a,b,c] - maximum[a,b,c], maximum[a,b,c])


type Pessoa = String
type Livro = String
type BancoDados = [(Pessoa,Livro)]

-- livros emprestados
baseExemplo :: BancoDados
baseExemplo = [("Sergio","O Senhor dos Aneis"),
               ("Andre","Duna"),
               ("Fernando","Jonathan Strange & Mr. Norrell"), 
               ("Fernando","Duna")]

-- exemplo: livros baseExemplo "Fernando" 
-- --> ["Jonathan Strange & Mr. Norrell","Duna"]
livros :: BancoDados -> Pessoa -> [Livro]
livros bd pss = [livro | (pessoa, livro) <- bd, pessoa == pss]

-- exemplo: emprestimos baseExemplo "Duna
-- --> ["Andre","Fernando"]
emprestimos :: BancoDados -> Livro ->[Pessoa]
emprestimos bd lvr = [pessoa | (pessoa, livro) <- bd, lvr == livro]

-- \parametro -> resultado
-- recebe (pessoa, livro) e retorna livro, se livro == lvr
emprestado :: BancoDados -> Livro -> Bool
emprestado bd lvr = any (\(pessoa, livro) -> livro == lvr) bd

qtdEmprestimos :: BancoDados -> Pessoa -> Int
qtdEmprestimos bd pss = length (filter (\(pessoa, livro) -> pessoa == pss) bd)

emprestar :: BancoDados -> Pessoa -> Livro -> BancoDados
emprestar bd pss lvr = [(pessoa, livro) | (pessoa, livro) <- bd, pessoa == pss && livro == lvr]

devolver :: BancoDados -> Pessoa -> Livro -> BancoDados
devolver bd pss lvr = [(pessoa, livro) | (pessoa, livro) <- bd, livro == lvr]