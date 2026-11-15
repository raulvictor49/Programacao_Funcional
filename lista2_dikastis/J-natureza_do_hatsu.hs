-- Definições de Tipos
data Nen = Fortificacao | Emissao | Transmutacao | Conjuracao | Manipulacao | Especializacao
         deriving (Read, Show, Eq, Ord)

data Reacao = AumentoVolume | MudancaCor | MudancaSabor | SurgimentoImpurezas | MovimentoFolha | ManifestacaoEspecial
            deriving (Read, Show, Eq)

data Condicao = Normal | EmperorTime | Especialista [Int]
              deriving (Read, Show, Eq)

data Componente = Componente Nen Int Int
                  deriving (Read, Show)

data Modalidade = Modalidade String [Componente]
                  deriving (Read, Show)

data Hatsu = Hatsu String [Modalidade]
             deriving (Read, Show)

data Usuario = Usuario String Reacao Int [(Nen, Int)] Condicao [Hatsu]
               deriving (Read, Show)

data Resultado = ForaDoAlcance [(Nen, Int, Int)]
               | FaltaDominio [(Nen, Int, Int)]
               | Executavel Int [(Nen, Int)]
               deriving (Read, Show, Eq)

data Situacao = Indisponivel | Parcial | Completo
              deriving (Read, Show, Eq)

data AnaliseHatsu = AnaliseHatsu String Situacao [(String, Resultado)]
                   deriving (Read, Show, Eq)

data Avaliacao = Avaliacao String Nen [AnaliseHatsu]
                deriving (Read, Show, Eq)

data Resumo = Resumo Nen Int Int Int [String]
              deriving (Read, Show, Eq)

data Relatorio = Relatorio [Avaliacao] [Resumo]
                 deriving (Read, Show, Eq)


-- Função auxiliar para remover duplicatas preservando a ordem
nubNen :: [Nen] -> [Nen]
nubNen [] = []
nubNen (x:xs) = x : nubNen (filter (/= x) xs)

-- Mapeia a Reação do Teste da Água para a categoria Nen correspondente
reacaoParaNen :: Reacao -> Nen
reacaoParaNen AumentoVolume        = Fortificacao
reacaoParaNen MudancaCor           = Emissao
reacaoParaNen MudancaSabor         = Transmutacao
reacaoParaNen SurgimentoImpurezas  = Conjuracao
reacaoParaNen MovimentoFolha       = Manipulacao
reacaoParaNen ManifestacaoEspecial = Especializacao

-- Retorna o percentual de afinidade baseando-se na linha e coluna da tabela de aprendizagem
afinidade :: Nen -> Nen -> Int
afinidade Fortificacao Fortificacao   = 100
afinidade Fortificacao Emissao        = 80
afinidade Fortificacao Transmutacao   = 80
afinidade Fortificacao Conjuracao     = 60
afinidade Fortificacao Manipulacao    = 60
afinidade Fortificacao Especializacao = 0

afinidade Emissao Fortificacao        = 80
afinidade Emissao Emissao             = 100
afinidade Emissao Transmutacao        = 60
afinidade Emissao Conjuracao          = 40
afinidade Emissao Manipulacao         = 80
afinidade Emissao Especializacao      = 0

afinidade Transmutacao Fortificacao   = 80
afinidade Transmutacao Emissao        = 60
afinidade Transmutacao Transmutacao   = 100
afinidade Transmutacao Conjuracao     = 80
afinidade Transmutacao Manipulacao    = 40
afinidade Transmutacao Especializacao = 0

afinidade Conjuracao Fortificacao     = 60
afinidade Conjuracao Emissao          = 40
afinidade Conjuracao Transmutacao     = 80
afinidade Conjuracao Conjuracao       = 100
afinidade Conjuracao Manipulacao      = 60
afinidade Conjuracao Especializacao   = 0

afinidade Manipulacao Fortificacao    = 60
afinidade Manipulacao Emissao         = 80
afinidade Manipulacao Transmutacao    = 40
afinidade Manipulacao Conjuracao      = 60
afinidade Manipulacao Manipulacao     = 100
afinidade Manipulacao Especializacao  = 0

afinidade Especializacao _            = 100

-- Índice numérico das categorias para mapear a lista de taxas de um Especialista
nenIdx :: Nen -> Int
nenIdx Fortificacao   = 0
nenIdx Emissao        = 1
nenIdx Transmutacao   = 2
nenIdx Conjuracao     = 3
nenIdx Manipulacao    = 4
nenIdx Especializacao = 5

-- Determina a afinidade base correta para calcular limites a depender da condição
limiteAfinidade :: Condicao -> Nen -> Nen -> Int
limiteAfinidade Normal catNat alvo          = afinidade catNat alvo
limiteAfinidade (Especialista _) _ alvo     = afinidade Especializacao alvo
limiteAfinidade EmperorTime _ alvo          = afinidade Conjuracao alvo

-- Calcula o limite de aprendizado de acordo com as regras de condição
getLimite :: Condicao -> Nen -> Int -> Nen -> Int
getLimite cond catNat nivelRef alvo = (nivelRef * limiteAfinidade cond catNat alvo) `div` 100

-- Calcula a eficiência de execução de acordo com as regras de condição
getEficiencia :: Condicao -> Nen -> Nen -> Int
getEficiencia Normal catNat alvo            = afinidade catNat alvo
getEficiencia (Especialista taxas) _ alvo   = taxas !! nenIdx alvo
getEficiencia EmperorTime _ alvo            = 100

-- Avalia uma modalidade retornando seus eventuais impedimentos ou sucesso
processaModalidade :: Condicao -> Nen -> Int -> [(Nen, Int)] -> Modalidade -> (String, Resultado)
processaModalidade cond catNat nivelRef dominios (Modalidade nome comps)
    | not (null foras)  = (nome, ForaDoAlcance foras)
    | not (null faltas) = (nome, FaltaDominio faltas)
    | otherwise         = (nome, Executavel resTotal efs)
  where
    catsUnicas = nubNen [ c | Componente c _ _ <- comps ]
    
    reqsConsolidados = [ (c, maximum [ req | Componente c' req _ <- comps, c' == c ]) | c <- catsUnicas ]
    
    foras = [ (c, req, getLimite cond catNat nivelRef c) 
            | (c, req) <- reqsConsolidados, req > getLimite cond catNat nivelRef c ]
    
    acharDominio (Just d) = d
    acharDominio Nothing  = 0
    
    faltas = [ (c, req, acharDominio (lookup c dominios)) 
             | (c, req) <- reqsConsolidados, req > acharDominio (lookup c dominios) ]
             
    efs = [ (c, getEficiencia cond catNat c) | c <- catsUnicas ]
    resTotal = sum [ (v * getEficiencia cond catNat c) `div` 100 | Componente c _ v <- comps ]

-- Analisa um Hatsu com base nas modalidades avaliadas utilizando guardas
processaHatsu :: Condicao -> Nen -> Int -> [(Nen, Int)] -> Hatsu -> AnaliseHatsu
processaHatsu cond catNat nivelRef dominios (Hatsu nome mods) =
    AnaliseHatsu nome sit resMods
  where
    resMods = map (processaModalidade cond catNat nivelRef dominios) mods
    execs   = length [ 1 | (_, Executavel _ _) <- resMods ]
    
    sit | execs == 0           = Indisponivel 
        | execs == length mods = Completo 
        | otherwise            = Parcial

-- Processa as avaliações individuais de um usuário
processaUsuario :: Usuario -> Avaliacao
processaUsuario (Usuario nome reacao nivelRef dominios cond hatsus) =
    Avaliacao nome catNat analises
  where
    catNat   = reacaoParaNen reacao
    analises = map (processaHatsu cond catNat nivelRef dominios) hatsus

-- Gera o resumo final para cada Categoria do Nen
geraResumo :: [Avaliacao] -> Nen -> Resumo
geraResumo avaliacoes cat = 
    Resumo cat qtdUsers totComp totExec destaques
  where
    avsDaCat = [ av | av@(Avaliacao _ c _) <- avaliacoes, c == cat ]
    
    statsUsers = [ (nome, 
                    length [1 | AnaliseHatsu _ Completo _ <- ans], 
                    sum [1 | AnaliseHatsu _ _ resMods <- ans, (_, Executavel _ _) <- resMods]) 
                 | Avaliacao nome _ ans <- avsDaCat ]
    
    qtdUsers = length avsDaCat
    totComp  = sum [ hc | (_, hc, _) <- statsUsers ]
    totExec  = sum [ me | (_, _, me) <- statsUsers ]
    
    maxScore | null statsUsers = (0, 0)
             | otherwise       = maximum [ (hc, me) | (_, hc, me) <- statsUsers ]
             
    destaques = [ nome | (nome, hc, me) <- statsUsers, (hc, me) == maxScore ]

-- Função Principal do Sistema
analisarUsuarios :: [Usuario] -> Relatorio
analisarUsuarios usuarios =
    Relatorio avaliacoes resumos
  where
    avaliacoes       = map processaUsuario usuarios
    catsApresentadas = nubNen [ cat | Avaliacao _ cat _ <- avaliacoes ]
    resumos          = map (geraResumo avaliacoes) catsApresentadas