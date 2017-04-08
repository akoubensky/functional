{-
 Тестирование функций работы с "застежкой" списка
 -}
module Main where

import Position
import Data.Maybe(fromJust)

-- Применение функции с переставленными аргументами
(>>>) :: a -> (a -> b) -> b
(>>>) = flip ($)
infixl 1 >>>

-- Применение функции к функтору с переставленными аргументами
(<&>) :: Functor m => m a -> (a -> b) -> m b
(<&>) = flip fmap
infixl 1 <&>

-- Удаление отрицательных элементов из списка весьма неестественным образом
removeNegatives :: (Ord a, Num a) => [a] -> [a]
-- removeNegatives = filter (>=0)
removeNegatives list = start list >>> remNegs >>> get
    where
      remNegs :: (Ord a, Num a) => Pos a -> Pos a
      remNegs pos = find (<0) pos   -- ищем отрицательное
                    >>= remove      -- удаляем, если нашли
                    >>> maybe pos (remNegs . snd)   -- продолжаем или останавливаемся

-- Удвоение всех элементов списка, меньших заданного
doubleSmalls :: (Ord a, Num a) => a -> [a] -> [a]
doubleSmalls limit list = start list >>> double >>> get
    where
      double pos = find (<limit) pos        -- ищем маленькое
                   >>= change (*2)          -- удваиваем, если нашли
                   >>= right                -- сдвигаем вправо
                   >>> maybe pos double     -- продолжаем или останавливаемся

test1 = start [1..10]   -- ([],[1,2,3,4,5,6,7,8,9,10])
       >>> rightN 5     -- ([1,2,3,4,5],[6,7,8,9,10])
       <&> insert 20    -- ([1,2,3,4,5],[20,6,7,8,9,10])
       >>= leftN 2      -- ([1,2,3],[4,5,20,6,7,8,9,10])
       >>= change (*10) -- ([1,2,3],[40,5,20,6,7,8,9,10])
       <&> insert 30    -- ([1,2,3],[30,40,5,20,6,7,8,9,10])
       <&> toEnd        -- ([1,2,3,30,40,5,20,6,7,8,9,10],[])
       >>= left         -- ([1,2,3,30,40,5,20,6,7,8,9],[10])
       >>= remove       -- ([1,2,3,30,40,5,20,6,7,8,9],[])
       <&> snd 
       <&> get
       >>> maybe [] id
test2 = removeNegatives [3,5,-2,0,-12,2,6,-1]
test3 = doubleSmalls 5 [2,4,6,8,1,3,5,7,9,2]

main = do
    print test1
    print test2
    print test3