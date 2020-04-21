module Main where

import Activacion

convergencia esperado real = esperado == real

main = do
    let theta = 0.2
    let alpha = 0.2 

    let tabla = [([0,0,0],0), ([0,0,1], 1), ([0,1,0], 1), ([0,1,1], 1), ([1,0,0], 1), ([1,0,1], 1), ([1,1,0], 1), ([1,1,1], 1)]
    let pesos = [[0.2,0.2,0.2],[0.2,0.2,0.2],[0.2,0.2,0.2],[0.2,0.2,0.2],[0.2,0.2,0.2],[0.2,0.2,0.2],[0.2,0.2,0.2],[0.2,0.2,0.2]]

    let salidas = [salida (fst x) y theta | (x, y) <- zip tabla pesos]