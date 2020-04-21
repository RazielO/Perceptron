module Pesos where

calcularError esperado real = esperado - real

delta alpha x e = alpha * x * e

nuevoPeso w alpha x esperado real = w + delta alpha x (calcularError esperado real)