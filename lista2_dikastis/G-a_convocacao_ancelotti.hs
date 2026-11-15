import Data.List (partition, sortBy)

data Posicao = Goleiro | Lateral | Zagueiro | Volante | Meia | Atacante
    deriving (Read, Show, Eq, Ord)

data Jogador = Jogador String Posicao Double
    deriving (Read, Show, Eq)

data Comando = Convocar String 
             | Dispensar String 
             | SubstituirPior Posicao 
             | Lesionar String
    deriving (Read, Show)

-- Ordena do maior para o menor (decrescente)
cmpNotaDesc :: Jogador -> Jogador -> Ordering
cmpNotaDesc (Jogador _ _ n1) (Jogador _ _ n2) = compare n2 n1

-- Ordena do menor para o maior (crescente)
cmpNotaAsc :: Jogador -> Jogador -> Ordering
cmpNotaAsc (Jogador _ _ n1) (Jogador _ _ n2) = compare n1 n2

-- Função principal
montarSelecao :: [Jogador] -> [Comando] -> [Jogador]
montarSelecao poolInicial comandos = 
    -- foldl passa a tupla inicial (poolInicial, []) por todos os comandos
    let (_, selecaoFinal) = foldl executar (poolInicial, []) comandos
    in sortBy cmpNotaDesc selecaoFinal -- A seleção final é ordenada por nota decrescente

-- Processa cada comando atualizando a tupla (pool, selecao)
executar :: ([Jogador], [Jogador]) -> Comando -> ([Jogador], [Jogador])

executar (pool, sel) (Convocar n) =
    -- partition separa quem tem o nome 'n' do resto da lista
    let (alvos, restoPool) = partition (\(Jogador nome _ _) -> nome == n) pool
    in if null alvos 
       then (pool, sel) 
       else (restoPool, head alvos : sel) -- Move do pool para a seleção

executar (pool, sel) (Dispensar n) =
    let (alvos, restoSel) = partition (\(Jogador nome _ _) -> nome == n) sel
    in if null alvos 
       then (pool, sel) 
       else (head alvos : pool, restoSel) -- Move da seleção para o pool

executar (pool, sel) (Lesionar n) =
    -- Remove o jogador 'n' permanentemente de ambos os lados
    let remover = filter (\(Jogador nome _ _) -> nome /= n)
    in (remover pool, remover sel)

executar (pool, sel) (SubstituirPior p) =
    -- Filtra apenas os jogadores da posição P em ambos os lados
    let poolPos = filter (\(Jogador _ pos _) -> pos == p) pool
        selPos  = filter (\(Jogador _ pos _) -> pos == p) sel
    in if null poolPos || null selPos 
       then (pool, sel) -- Se faltar jogador em algum dos lados, não faz nada
       else 
           -- Pega o pior da seleção e o melhor do pool
           let piorSel    = head (sortBy cmpNotaAsc selPos)
               melhorPool = head (sortBy cmpNotaDesc poolPos)
               -- Remove o pior da seleção e adiciona o melhor
               novaSel  = melhorPool : filter (/= piorSel) sel
               -- Remove o melhor do pool e devolve o pior
               novoPool = piorSel : filter (/= melhorPool) pool
           in (novoPool, novaSel)