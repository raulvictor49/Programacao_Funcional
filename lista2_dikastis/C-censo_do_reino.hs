import Data.List (sortBy)

data Provincia = Provincia String [Cidade] deriving (Read, Show)
-- Nome, População, Area, Renda
data Cidade = Cidade String Int Double Int deriving (Read, Show)

data GrupoAnalise = Provincias | Cidades deriving (Read, Show, Eq)
data ObjetivoAnalise = Densidade | RendaPerCapita deriving (Read, Show, Eq)
data OrdemAnalise = Crescente | Decrescente deriving (Read, Show, Eq)
data Analise = Analise GrupoAnalise ObjetivoAnalise OrdemAnalise deriving (Read, Show)

analisarCenso :: [Provincia] -> Analise -> [String]
analisarCenso provincias (Analise grupo objetivo ordem) =
    let 
        -- Extrai e calcula os pares (Nome, Valor) dependendo do Grupo (Cidades ou Províncias)
        dados = case grupo of
            Cidades -> 
                let todasAsCidades = concatMap (\(Provincia _ cidades) -> cidades) provincias
                in map (\c@(Cidade nome _ _ _) -> (nome, valorCidade objetivo c)) todasAsCidades
            
            Provincias -> 
                map (\p@(Provincia nome _) -> (nome, valorProvincia objetivo p)) provincias
        
        -- Ordena sempre de forma crescente primeiro
        dadosOrdenados = sortBy (\(_, valor1) (_, valor2) -> compare valor1 valor2) dados
        
        -- Aplica a inversão caso a ordem exigida seja Decrescente
        resultadoFinal = case ordem of
            Crescente   -> dadosOrdenados
            Decrescente -> reverse dadosOrdenados
            
    -- Extrai apenas os nomes
    in map fst resultadoFinal

-- Função auxiliar para calcular o valor alvo de uma única Cidade
valorCidade :: ObjetivoAnalise -> Cidade -> Double
valorCidade Densidade (Cidade _ pop area _) = 
    fromIntegral pop / area
valorCidade RendaPerCapita (Cidade _ pop _ renda) = 
    fromIntegral renda / fromIntegral pop

-- Função auxiliar para calcular o valor alvo de uma Província inteira (agregando os dados)
valorProvincia :: ObjetivoAnalise -> Provincia -> Double
valorProvincia objetivo (Provincia _ cidades) = 
    let popTotal   = sum [pop   | Cidade _ pop _ _ <- cidades]
        areaTotal  = sum [area  | Cidade _ _ area _ <- cidades]
        rendaTotal = sum [renda | Cidade _ _ _ renda <- cidades]
    in case objetivo of
        Densidade      -> fromIntegral popTotal / areaTotal
        RendaPerCapita -> fromIntegral rendaTotal / fromIntegral popTotal