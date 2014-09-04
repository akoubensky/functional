module Fib where

-- Вычисление числа Фибоначчи, заданного порядковым номером.
-- "Плохая" рекурсия.
fib        :: Integer -> Integer
fib 1      =  1
fib 2      =  1
fib n      =  fib (n-1) + fib (n-2)
