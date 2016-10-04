{-
  Обход в ширину графа, представленного списками смежности.
-}

module BFS where

import Data.List(union, (\\))

-- Граф представлен списком списков смежности. Длина списка списков соответствует числу вершин графа.
-- В списке с номерм i содержатся номера вершин, в которые ведут дуги из вершины с номером i.
newtype Graph = Graph [[Int]]

-- Обход в ширину, начиная с заданной вершины. Ищем список всех вершин, достижимых из заданной.
-- Аргументы:
--      u - начальная вершина
--      (Graph g) - исходный граф
-- Результат: список достижимых вершин.
bfs :: Int -> Graph -> [Int]
bfs u (Graph g) = bfs' [] [u]  -- формируем список пройденных вершин (в начале пустой)
                               -- и "фронт волны" ( в начале - тоже только начальная вершина)
    where
        -- Вспомогательная рекурсивная функция с накапливающими аргументами осуществляет всю работу.
        -- Аргументы:
        --      passed - список пройденных вершин;
        --      front - "фронт волны".
        -- Результат: конечный список пройденных вершин.
        bfs' passed [] = passed    -- фронт волны пуст, все достижимые вершины пройдены
        bfs' passed front = bfs' newPassed newFront
            where
                newPassed = union passed front    -- в новый список пройденных вершин добавляем фронт фолны
                newFront = concatMap (g!!) front \\ newPassed -- формируем новый фронт из еще не пройденных вершин.

test = Graph [[0,2,4,5],[2,3],[3,4],[5],[1],[3]]