import System.Random 

numeroR =  do
    
    return $ fst $ randomR(0.5::Float,-0.5::Float) newStdGen

pesos lista cantidad
    | length lista == cantidad = lista
    | otherwise = pesos (lista ++ [numeroR]) cantidad