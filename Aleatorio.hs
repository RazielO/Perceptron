module Aleatorio where

import System.Random
import Control.Monad (replicateM)

numeroR =  do
    newStdGen
    g <- getStdGen
    return $ fst $ randomR(0.5::Float,-0.5::Float) g

aleatorios can = replicateM can numeroR

generarPesos cantidad entradas = replicateM cantidad $ aleatorios entradas
