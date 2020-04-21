module Activacion where

sumatoria entradas pesos theta = sum $ [(x * w) - theta | (x, w) <- (zip entradas pesos)]

paso x
    | x >= 0 = 1
    | otherwise = 0

salida entradas pesos theta = paso $ sumatoria entradas pesos theta