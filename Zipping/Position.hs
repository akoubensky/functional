{-
 Сохранение позиции в списке при выполнении последовательных действий
 в локальной области длинного списка
 -}
module Position(
    Pos,            -- "застежка" позиции в списке с путем до нее
    start,          -- образовать "застежку" из списка
    get,            -- извлечь список из застежки
    toBegin,        -- переместить позицию в начало списка
    toEnd,          -- переместить позицию в конец списка
    startOfList,    -- "застежка" в начале списка?
    endOfList,      -- "застежка" в конце списка?
    left,           -- сдвиг позиции влево не один шаг
    right,          -- сдвиг позиции вправо не один шаг
    leftN,          -- сдвиг позиции влево не несколько шагов
    rightN,         -- сдвиг позиции вправо не несколько шагов
    find,           -- найти позицию элемента в списке
    insert,         -- вставить элемент в текущей позиции
    remove,         -- удалить элемент в текущей позиции
    change          -- преобразовать элемент в текущей позиции
    ) where

import Data.Maybe(fromJust)

newtype Pos a = Pos ([a], [a]) deriving Show

start :: [a] -> Pos a
start list = Pos ([], list)

get :: Pos a -> [a]
get p = case toBegin p of Pos (_, ls) -> ls

toBegin :: Pos a -> Pos a
toBegin (Pos (path, ls)) = Pos ([], foldl (flip (:)) ls path)

toEnd :: Pos a -> Pos a
toEnd (Pos (path, ls)) = (Pos (foldl (flip (:)) path ls, []))

startOfList :: Pos a -> Bool
startOfList (Pos (path, _)) = null path

endOfList :: Pos a -> Bool
endOfList (Pos (_, ls)) = null ls

left :: Pos a -> Maybe (Pos a)
left (Pos ([], _)) = Nothing
left (Pos ((x:sx), ls)) = Just $ Pos (sx, (x:ls))

right :: Pos a -> Maybe (Pos a)
right (Pos (_, [])) = Nothing
right (Pos (path, (x:sx))) = Just $ Pos ((x:path), sx)

leftN :: Int -> Pos a -> Maybe (Pos a)
leftN 0 pos = Just pos
leftN n pos = left pos >>= leftN (n-1)

rightN :: Int -> Pos a -> Maybe (Pos a)
rightN 0 pos = Just pos
rightN n pos = right pos >>= rightN (n-1)

find :: (a -> Bool) -> Pos a -> Maybe (Pos a)
find p (Pos (_, [])) = Nothing
find p pos@(Pos (path, (x:xs))) | p x = Just pos
                                | otherwise = right pos >>= find p

insert :: a -> Pos a -> Pos a
insert e (Pos (path, ls)) = Pos (path, (e:ls))

remove :: Pos a -> Maybe (a, Pos a)
remove (Pos (_, [])) = Nothing
remove (Pos (path, (x:xs))) = Just (x, Pos (path, xs))

change :: (a -> a) -> Pos a -> Maybe (Pos a)
change _ (Pos (_, [])) = Nothing
change f (Pos (prev, (x:next))) = Just $ Pos (prev, (f x:next))