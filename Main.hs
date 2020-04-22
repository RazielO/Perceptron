module Main where

import Activacion
import Pesos
import Aleatorio

convergencia esperado real = esperado == real

entrenamiento :: Float -> Float -> [[Float]] -> [([Int], Int)] -> [Int]
entrenamiento theta alpha pesos tabla = do
    let salidas = [salida (fst x) y theta | (x, y) <- zip tabla pesos] :: [Int]
    let esperados = map snd tabla
    if convergencia esperados salidas
        then salidas
        else entrenamiento theta alpha (map (\i -> nuevosPesos (fst $ tabla !! i) (pesos !! i) alpha (snd $ tabla !! i) (salidas !! i)) [0..(length tabla - 1)]) tabla

main = do
    let tabla = [([0,0,0],0), ([0,0,1], 1), ([0,1,0], 1), ([0,1,1], 1), ([1,0,0], 1), ([1,0,1], 1), ([1,1,0], 1), ([1,1,1], 1)]

    theta <- numeroR
    pesos <- generarPesos (length tabla) (length $ fst $ tabla !! 0)
    let alpha = 0.1

    print $ entrenamiento theta alpha pesos tabla

