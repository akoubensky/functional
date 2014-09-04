module Height where

{- Тип данных - однородное двоичное дерево -}
data Tree a = Empty |
              Node (Tree a) a (Tree a)

{- Вычисление высоты дерева -}              
height  ::  Tree a -> Int
height Empty           =  0
height (Node t1 _ t2)  =  1 + max (height t1) (height t2)

{- Пример дерева высоты 3 -}
myTree  ::  Tree Char
myTree  = Node (Node 
                  (Node Empty 'D' Empty)
                  'B'
                  Empty)
               'A'
               (Node
                  (Node Empty 'E' Empty)
                  'C'
                  (Node Empty 'F' Empty))
