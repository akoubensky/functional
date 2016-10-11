module BellmanFord where

import Graph
import Data.Maybe

-- список дуг графа
type ArcList w = [Arc w]
-- функция, выдающая оценку найденного минимального раастояния до заданной вершины
type Weights w = Int -> Maybe w

-- Список минимальных расстояний до вершин, сформированный из функции оценки.
distances :: Graph w -> Weights w -> [Maybe w]
distances g weights = map weights [0..verts g - 1]

-- Функция формирования начальноых оценок минимальных расстояний от заданной вершины.
-- Расстояния до всех вершин, кроме начальной, в первый момент не определены.
initDistances :: Num w => Int -> Weights w
initDistances u = replace u 0 (\v -> Nothing)  -- replace заменяет значение для начальной вершины на нулевое

-- Функция изменения оценки расстояния до заданной вершины.
-- Аргументы:
--     - номер вершины, для которой изменяется оценка;
--     - новое значение оценки минимального расстояния;
--     - текущая функция оценки.
-- Результат: новая функция оценки
replace :: Int -> w -> Weights w -> Weights w
replace u newValue weights v = if u == v then Just newValue else weights v

-- Функция релаксации дуги
-- Аргументы:
--     - текущая функция оценки минимальных расстояний до вершин;
--     - дуга;
-- Результат: пара из признака того, что оценки изменились, и новой функции оценки.
relax :: Real w => Weights w -> Arc w -> (Bool, Weights w)
relax weights arc =
        if isJust wFrom && (isNothing wTo || fromJust wFrom + w < fromJust wTo)
        then (True, replace t (fromJust wFrom + w) weights)  -- улучшаем оценку
        else (False, weights)                                -- оценка не изменяется
    where (f, w, t) = (from arc, weight arc, to arc)
          (wFrom, wTo) = (weights f, weights t)

-- Функция, реализующая один проход по дугам в алгоритме Беллмана - Форда.
-- Все дуги подвергаются релаксации, и проверяется, изменилась ли хоть одна оценка.
-- Аргументы:
--     - текущая функция оценки минимальных расстояний до вершин;
--     - исходный граф.
-- Результат: пара из признака того, что оценки изменились, и новой функции оценки.
bfCycle ::Real w => Weights w -> Graph w -> (Bool, Weights w)
bfCycle weights g = gfold func (False, weights) g
    where func (ch, weights) arc = (ch || newCh, newWeights)
              where (newCh, newWeights) = relax weights arc

-- Реализация алгоритма Беллмана - Форда
-- Функция формирует начальную оценку минимальных расстояний до вершин и последовательно
-- выполняет циклы релаксации дуг до тех пор пока либо оценки перестанут улучшаться,
-- либо будет выполнено уже слишком много циклов.
-- Аргументы:
--     - номер начальной вершины, расстояния от которой ищутся;
--     - исходный граф
-- Результат: список найденных минимальных расстояний.
bellmanFord :: Real w => Int -> Graph w -> [Maybe w]
bellmanFord start g = if changed
                      then []    -- работа закончена в состоянии, когда что-то еще меняется.
                      else distances g final
    where n = verts g
          (changed, _, final) = steps (0, initDistances start)
          -- Функция steps осуществляет шаги циклической релаксации до завершения работы алгоритма.
          steps (k, weights) | k > n = (True, n, weights)       -- очевидно, есть цикл с суммарной отрицательной нагрузкой;
                             | ch = steps (k+1, newWeights)     -- что-то изменилось, продолжаем работу
                             | otherwise = (False, k, weights)  -- стабилизация достигнута, заканчиваем работу
             where (ch, newWeights) = bfCycle weights g

-- Тестовый маленький граф
testGraph :: Graph Int
testGraph =
    addArc (0,-2,1) $ addArc (1,2,2) $
    addArc (1,5,4) $ addArc (2,2,5) $
    addArc (2,4,3) $ addArc (4,1,2) $
    addArc (4,2,5) $ addArc (4,3,0) $
    addArc (5,1,3) $ addArc (2,-1,1) $
    createGraph 6