module Graph(
        Graph,          -- тип данных для представления графа
        Arc,            -- тип данных для представления дуги графа
        verts,          -- функция вычисления количества вершин графа
        arcs,           -- функция вычисления списка дуг графа
        from,           -- вершина, из которой исходит дуга
        to,             -- вершина, в которую входит дуга
        weight,         -- вес дуги
        createGraph,    -- создание пустого графа
        addArc,         -- добавление дуги в граф
        gmap,           -- отображение, определенное для нагрузок на дуги графа
        gfold           -- свертка по дугам графа
        ) where

newtype Graph w = Graph[[(w, Int)]] deriving Show -- представление списками смежности
newtype Arc w   = Arc (Int, w, Int) deriving Show -- начало дуги, нагрузка на дугу, конец дуги

-- число вершин графа - количество списков смежности в его представлении
verts :: Graph w -> Int
verts (Graph list) = length list

-- список дуг графа. Дуги формируются припиысыванием номера списка смежности
-- к представлению дуги в списке смежности
arcs :: Graph w -> [Arc w]
arcs (Graph list) = concat $ zipWith addStart [0..] list
    where addStart u = map (\(w, v) -> Arc (u, w, v))

-- Функции доступа к копонентам дуги: from, weight и to
from, to :: Arc w -> Int
weight :: Arc w -> w
from (Arc (f,_,_)) = f
weight (Arc (_,w,_)) = w
to (Arc (_,_,t)) = t

-- Создание пустого графа (графа без дуг) из заданного числа вершин.
-- Создается заданное количество пустых списков смежности.
createGraph :: Int -> Graph w
createGraph n = Graph $ replicate n []

-- Добавление дуги в граф. Не проверяется, существует ли уже такая дуга
-- (фактически можно создать мультиграф), но проверяется корректность номеров вершин
addArc :: (Int, w, Int) -> Graph w -> Graph w
addArc (from, w, to) (Graph list)
        | from < 0 || to < 0 || from >= n || to >= n = error "Vertex does not exist"
        | otherwise = Graph $ prev ++ (((w, to) : arcs) : next)
    where n = length list
          (prev, arcs:next) = splitAt from list

-- Функция gmap реализует отображение на нагрузках на дуги графа.
gmap :: (v -> w) -> Graph v -> Graph w
gmap f (Graph list) = Graph $ map mapFunc list
    where mapFunc arcs = map (\(w, v) -> (f w, v)) arcs

-- Функция свертки по дугам графа
gfold :: (b -> Arc w -> b) -> b -> Graph w  -> b
gfold f seed g = foldl f seed $ arcs g