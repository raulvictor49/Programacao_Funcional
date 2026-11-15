data Recurso = Alcool | Pano | Lamina
    deriving (Read, Show, Eq)

data Inimigo = Corredor | Estalador | Baiacu
    deriving (Read, Show, Eq)

data Acao = Coletar Recurso Int
          | FabricarKit
          | FabricarFaca
          | Curar
          | Enfrentar Inimigo
          deriving (Read, Show)


-- Função auxiliar para extrair a quantidade inicial de um recurso do inventário
pegarQtd :: Recurso -> [(Recurso, Int)] -> Int
pegarQtd _ [] = 0
pegarQtd r ((rec, q):xs)
    | r == rec  = q
    | otherwise = pegarQtd r xs


-- Função principal recursiva que processa ação por ação e atualiza o estado de Ellie
-- Estado: (Vida, Alcool, Pano, Lamina, Kits, Facas, Inimigos Derrotados, Lista de Ações)
processar :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> [Acao] -> (Int, Int)

-- Condição de morte: se a vida é <= 0, ignora as demais ações
processar 0 _ _ _ _ _ def _ = (0, def)

-- Caso base de sucesso: sem mais ações a processar
processar hp _ _ _ _ _ def [] = (hp, def)

-- Coleta de Recursos
processar hp al pa la k f def (Coletar Alcool q : acoes) =
    processar hp (al + q) pa la k f def acoes
    
processar hp al pa la k f def (Coletar Pano q : acoes) =
    processar hp al (pa + q) la k f def acoes
    
processar hp al pa la k f def (Coletar Lamina q : acoes) =
    processar hp al pa (la + q) k f def acoes

-- Fabricação
processar hp al pa la k f def (FabricarKit : acoes)
    | al >= 1 && pa >= 1 = processar hp (al - 1) (pa - 1) la (k + 1) f def acoes
    | otherwise          = processar hp al pa la k f def acoes
    
processar hp al pa la k f def (FabricarFaca : acoes)
    | la >= 2   = processar hp al pa (la - 2) k (f + 1) def acoes
    | otherwise = processar hp al pa la k f def acoes

-- Cura
processar hp al pa la k f def (Curar : acoes)
    | k >= 1    = processar (min 100 (hp + 40)) al pa la (k - 1) f def acoes
    | otherwise = processar hp al pa la k f def acoes

-- Enfrentamentos
processar hp al pa la k f def (Enfrentar Estalador : acoes)
    | f >= 1    = processar hp al pa la k (f - 1) (def + 1) acoes
    | hp > 35   = processar (hp - 35) al pa la k f (def + 1) acoes
    | otherwise = (0, def)  -- Vida não resiste ao dano (<= 35), Ellie morre
    
processar hp al pa la k f def (Enfrentar Corredor : acoes)
    | hp > 15   = processar (hp - 15) al pa la k f (def + 1) acoes
    | otherwise = (0, def)
    
processar hp al pa la k f def (Enfrentar Baiacu : acoes)
    | hp > 60   = processar (hp - 60) al pa la k f (def + 1) acoes
    | otherwise = (0, def)


sobreviver :: Int -> [(Recurso, Int)] -> [Acao] -> (Int, Int)
sobreviver hp inicial invAcoes =
    processar hp
              (pegarQtd Alcool inicial)
              (pegarQtd Pano inicial)
              (pegarQtd Lamina inicial)
              0 -- Quantidade inicial de Kits Médicos
              0 -- Quantidade inicial de Facas
              0 -- Quantidade inicial de inimigos derrotados
              invAcoes