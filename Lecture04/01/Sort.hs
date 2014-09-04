module Sort where

{- Тип данных - однородное двоичное дерево -}
data Tree a = Empty |
              Node (Tree a) a (Tree a)

{- Сортировка сводится к "разглаживанию" построенного по
   исходному списку двоичного дерева поиска -}
sort    :: (Ord a) => [a] -> [a]
sort ls       =  flatten (build ls)

{- Построение дерева по списку. Элементы по одному
   вставляются в дерево поиска (вначале - пустое) -}
build   :: (Ord a) => [a] -> Tree a
build list    =  foldr insert Empty list

{- Вставка элемента в дерево поиска производится
   по стандартному рекурсивному алгоритму. -}
insert  :: (Ord a) => a -> Tree a -> Tree a
insert e Empty                    =  Node Empty e Empty
insert e (Node t1 n t2) | e < n   =  Node (insert e t1) n t2
                        | e >= n  =  Node t1 n (insert e t2)

{- Свертка дерева с проходом справа налево -}
foldTree  ::  (a -> b -> b) -> b -> Tree a -> b
foldTree _ seed Empty           =  seed
foldTree f seed (Node t1 n t2)  =  foldTree f (f n (foldTree f seed t2)) t1

{- Разглаживание дерева - эффективная функция с использованием
   свертки дерева -}
flatten :: Tree a -> [a]
flatten tree  =  foldTree (:) [] tree

{- Функция get строит список в таком порядке элементов, что при последовательной
   вставке их в дерево поиска получается оптимальное дерево.
   Исходный список предполагается упорядоченным. -}
get :: (Ord a) => [a] -> [a]
get [] = []
get lst = mid : ((get fst) ++ (get snd))
             where (fst, (mid:snd)) = splitAt (length lst `div` 2) lst