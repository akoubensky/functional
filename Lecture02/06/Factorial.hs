module Factorial where

-- Вычисление факториала числа методом введения дополнительных аргументов
factorial  :: Integer -> Integer
factorial' :: Integer -> Integer -> Integer
factorial n = factorial' n 1               -- (factorial' n f) == (f * n!)
factorial' n f | n == 0  =  f
               | n > 0   =  factorial' (n-1) (n*f)