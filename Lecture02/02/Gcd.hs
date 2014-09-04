module Gcd where

import Prelude hiding (gcd)

-- Вычисление наибольшего общего делителя двух натуральных чисел
-- с помощью алгоритма Евклида
gcd              :: Integer -> Integer -> Integer 
gcd m n | m < n  =  gcd n m 
        | n < 0  =  error "gcd: Wrong argument" 
gcd m 0          =  m 
gcd m n          =  gcd n (m `mod` n) 