module Activacion where

sumatoria :: [Int] -> [Float] -> Float -> Float
sumatoria entradas pesos theta = sum $ [((fromIntegral x) * w) - theta | (x, w) <- (zip entradas pesos)]

paso :: Float -> Int
paso x
    | x >= 0 = 1
    | otherwise = 0

salida :: [Int] -> [Float] -> Float -> Int
salida entradas pesos theta = paso $ sumatoria entradas pesos theta
