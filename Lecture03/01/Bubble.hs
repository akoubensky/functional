module Bubble where

{- Вставка элемента в упорядоченный список -}
insert          :: (Ord a) => a -> [a] -> [a]
insert elem []  = [elem]
insert elem list@(x:s) | elem < x   = elem:list
                       | otherwise  = x:(insert elem s)

{- Сортировка осуществляется последовательной вставкой
   элементов в упорядоченную часть списка
 -}                       
bubble          :: (Ord a) => [a] -> [a]
bubble []       = []
bubble (x:s)    = insert x (bubble s)
                       