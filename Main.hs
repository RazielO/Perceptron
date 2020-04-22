module Main where

import Activacion
import Pesos

convergencia esperado real = esperado == real

entrenamiento :: Float -> Float -> [[Float]] -> [([Int], Int)] -> [Int]
entrenamiento theta alpha pesos tabla = do
    let salidas = [salida (fst x) y theta | (x, y) <- zip tabla pesos] :: [Int]
    let esperados = map snd tabla
    if convergencia esperados salidas
        then salidas
        else entrenamiento theta alpha (map (\i -> nuevosPesos (fst $ tabla !! i) (pesos !! i) alpha (snd $ tabla !! i) (salidas !! i)) [0..(length tabla - 1)]) tabla

main = do
    let theta = 0.2
    let alpha = 0.2

    let tabla = [([0,0,0],0), ([0,0,1], 1), ([0,1,0], 1), ([0,1,1], 1), ([1,0,0], 1), ([1,0,1], 1), ([1,1,0], 1), ([1,1,1], 1)]
    let pesos = [[0.2 :: Float, 0.2 :: Float, 0.2 :: Float] | x <- [1..8]]

    print $ entrenamiento theta alpha pesos tabla

