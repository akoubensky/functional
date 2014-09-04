module Factorial where

-- Факториал числа
factorial :: Integer -> Integer
factorial n | n == 0  = 1
            | n > 0   = n * (factorial (n-1))
