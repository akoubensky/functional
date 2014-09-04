-- Проверка заданного натурального числа на простоту

module Prime where

prime             :: Integer -> Bool 
prime'            :: Integer -> Integer -> Bool 
prime p    | p <= 0          =  error "prime: Non-positive argument"
           | otherwise       =  prime' 2 p 
prime' d p | d * d > p       =  True
           | p `mod` d == 0  =  False
           | otherwise       =  prime' (d+1) p 
