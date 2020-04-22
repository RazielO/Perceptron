module Salida where

import Data.List
import Text.Printf

header :: Int
header = 10

stringSize :: [Char]
stringSize = "%-" ++ (show header) ++ "s"

redondear :: Float -> Int -> Float
redondear numero decimales = (fromInteger $ round $ numero * (10^decimales)) / (10.0^^decimales)

imprimirLista lista = concat $ intersperse "," (map show lista)

formatoInicial = "| " ++ stringSize ++ " | "
formatoCentral = stringSize ++ " | "
formatoFinal = stringSize ++ " |\n"

imprimirPaso :: (Int, ([[Float]], [Int])) -> Int -> IO ()
imprimirPaso paso i
    | i /= length (fst $ snd paso) = do
            let pesos = (map (\x -> redondear x 5) $ (fst $ snd paso) !! i)

            printf formatoInicial (show $ fst paso)
            mapM_ (\x -> printf formatoCentral (show x)) pesos
            printf formatoFinal (show $ (snd $ snd paso) !! 0)

            imprimirPaso paso (i + 1)
    | otherwise = putStrLn $ " " ++ (concat $ replicate ((length $ (fst $ snd paso) !! 0) + 2) $ concat $ replicate (header + 3) "-")

imprimirHeaders pesos = do
    printf formatoInicial "EPOCH"
    mapM_ (\x -> printf formatoCentral ("W" ++ (show x))) [1..pesos]
    printf formatoFinal "SALIDA"
    putStrLn $ " " ++ (concat $ replicate (pesos + 2) $ concat $ replicate (header + 3) "-")
