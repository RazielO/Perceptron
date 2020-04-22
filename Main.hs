module Main where

import Activacion
import Pesos
import Aleatorio
import Salida

convergencia :: [Int] -> [Int] -> Bool
convergencia esperado real = esperado == real

entrenamiento :: Float -> Float -> [[Float]] -> [([Int], Int)] -> Int -> [(Int, ([[Float]], [Int]))] -> [(Int, ([[Float]], [Int]))]
entrenamiento theta alpha pesos tabla epoch proceso = do
    let salidas = [salida (fst x) y theta | (x, y) <- zip tabla pesos] :: [Int]
    let esperados = map snd tabla
    let nuevos = (map (\i -> nuevosPesos (fst $ tabla !! i) (pesos !! i) alpha (snd $ tabla !! i) (salidas !! i)) [0..(length tabla - 1)])
    let paso = [(epoch, (nuevos, salidas))]

    if convergencia esperados salidas
        then (proceso ++ paso)
        else entrenamiento theta alpha nuevos tabla (epoch + 1) (proceso ++ paso)

main = do
    let tabla = [([0,0,0],0), ([0,0,1], 1), ([0,1,0], 1), ([0,1,1], 1), ([1,0,0], 1), ([1,0,1], 1), ([1,1,0], 1), ([1,1,1], 1)]

    theta <- numeroR
    pesos <- generarPesos (length tabla) (length $ fst $ tabla !! 0)
    let alpha = 0.2

    let proceso = entrenamiento (abs theta) alpha pesos tabla 1 []
    imprimirHeaders (length (fst $ tabla !! 0))
    mapM_ (\x -> imprimirPaso x 0) proceso

