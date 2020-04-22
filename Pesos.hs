module Pesos where

calcularError :: Int -> Int -> Int
calcularError esperado real = esperado - real

delta :: Float -> Float -> Int -> Float
delta alpha x e = alpha * x * (fromIntegral $ e)

nuevoPeso :: Float -> Float -> Int -> Int -> Int -> Float
nuevoPeso w alpha x esperado real = w + delta alpha (fromIntegral x) (fromIntegral $ calcularError esperado real)

--nuevosPesos :: [Int] -> [Float] -> Float -> Int -> Int -> [Float]
nuevosPesos entradas pesos alpha yd y = [nuevoPeso w alpha x yd y | (x, w) <- zip entradas pesos]
