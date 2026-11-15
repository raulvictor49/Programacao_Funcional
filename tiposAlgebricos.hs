-- Definição de um novo tipo para representar formas geométricas:
data Figura = Circulo Double
            | Retangulo Double Double

-- O construtor de tipo é Figura.
-- Osconstrutores de dados deste tipo são:
-- Circulo :: Double-> Figura
-- Retangulo :: Double-> Double-> Figura

-- e com eles é possível construir todo e qualquer valor do tipo Figura:
a :: Figura
a = Circulo 2.3-- um círculo de raio 2.3

b :: Figura
b = Retangulo 2.8 3.1-- um retângulo de base 2.8 e altura 3.1

lfig :: [Figura]
lfig = [Retangulo 5 3, Circulo 5.7, Retangulo 2 2]

-- Osconstrutores são utilizados em casamento de padrões para acessar os componentes de um valor do tipo algébrico.
-- Podemos definir funções envolvendo os tipos algébricos.

eRedondo :: Figura-> Bool
eRedondo (Circulo _) = True
eRedondo (Retangulo _ _) = False
-- ex:
-- eRedondo (Circulo 3.2) -- => True
-- eRedondo (Retangulo 2 5.1) -- => False

area :: Figura-> Double
area (Circulo r) = pi * r^2
area (Retangulo b a) = b * a
-- ex:
-- area (Circulo 2.5) -- => 19.634954084936208
-- area (Retangulo 2 5.1) -- => 10.2

quadrado :: Double-> Figura
quadrado lado = Retangulo lado lado
-- ex:
-- area (quadrado 2.5) -- => 6.25


--  Definição de um novo tipo para representar direções de movimento:
data Sentido = Esquerda | Direita | Acima | Abaixo

-- Oconstrutor de tipo é Sentido.
-- Osconstrutores de dados deste tipo, todos constantes, são:
-- Esquerda :: Sentido
-- Direita :: Sentido
-- Acima :: Sentido
-- Abaixo :: Sentido

-- Quandoosconstrutores de dados são constantes, (ou seja, não tem argumentos), dizemos que o tipo é uma enumeração.