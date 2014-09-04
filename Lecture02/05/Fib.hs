module Fib where

-- Вычисление n-го числа Фибоначчи методом введения дополнительных аргументов
fib   :: Integer -> Integer
fib'  :: Integer -> Integer -> Integer -> Integer -> Integer
fib'  n k fk fk1 | k == n  =  fk
                 | k < n   =  fib' n (k+1) (fk+fk1) fk  
fib 1 = 1
fib n = fib' n 2 1 1
