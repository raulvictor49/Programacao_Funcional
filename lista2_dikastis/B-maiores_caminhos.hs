data Tree t = NilT | Node t (Tree t) (Tree t)
            deriving (Show, Read)

caminhos :: Tree t -> [[t]]
caminhos NilT = []
caminhos (Node v NilT NilT) = [[v]] -- caso base: nó folha
caminhos (Node v esq dir) = map (v :) (caminhos esq ++ caminhos dir)

maioresCaminhos :: (Ord t, Num t) => Tree t -> [[t]]
maioresCaminhos NilT = []
maioresCaminhos t = filter (\c -> (sum c) == maiorCaminho) todosCaminhos
    where 
        todosCaminhos = caminhos t
        maiorCaminho = maximum (map sum todosCaminhos)
