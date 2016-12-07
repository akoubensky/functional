module CombCurry2 where

import ShowExpr

s, k, i, b, c :: Expression
s = Function "S"
k = Function "K"
i = Function "I"
b = Function "B"
c = Function "C"
y = Function "Y"

-- Функция абстрагирования от переменной. Вторым аргументом функции
-- является выражение, уже находящееся в комбинаторной форме
abstr :: String -> Expression -> Expression
abstr _ c@(Constant _) = Application k c
abstr _ f@(Function _) = Application k f
abstr x v@(Variable y) | x == y = i
                       | otherwise = Application k v
abstr x (Application e1 e2) = abstr' a1 a2
    where a1 = abstr x e1
          a2 = abstr x e2
          -- Вспомогательная функция, реализующая применение оптимизационных правил Карри
          abstr' (Application (Function "K") e) (Function "I") = e
          abstr' (Application (Function "K") e1)
                 (Application (Function "K") e2) =
              Application k (Application e1 e2)
          abstr' (Application (Function "K") e1) e2 =
              Application (Application b e1) e2 
          abstr' e1 (Application (Function "K") e2) =
              Application (Application c e1) e2 
          abstr' a1 a2 = Application (Application s a1) a2

comb :: Expression -> Expression
comb (Lambda x e) = abstr x $ comb e
comb (Application e1 e2) = Application (comb e1) (comb e2)
comb e = e

-- Правила применения комбинаторов (редукций)
eval :: Expression -> Expression
eval (Application e1 e2) = let func = eval e1; arg = eval e2 in
    case  func of
        (Function "I") -> arg
        (Application (Function "K") x) -> x
        (Application (Application (Function "S") f) g) -> eval (Application (Application f arg) (Application g arg))
        (Application (Application (Function "B") f) g) -> eval (Application f (Application g arg))
        (Application (Application (Function "C") f) arg2) -> eval (Application (Application f arg) arg2)
        (Function "Y") -> eval (Application arg (Application y arg))
        (Application (Function "+") (Constant n)) -> case arg of
            Constant m -> Constant (n + m)
            otherwise -> (Application func arg)
        (Application (Function "-") (Constant n)) -> case arg of
            Constant m -> Constant (n - m)
            otherwise -> (Application func arg)
        (Application (Function "*") (Constant n)) -> case arg of
            Constant m -> Constant (n * m)
            otherwise -> (Application func arg)
        (Function "eq0") -> case arg of
            Constant 0 -> Constant 1
            otherwise -> Constant 0
        (Application (Application (Function "if") (Constant 0)) e) -> arg
        (Application (Application (Function "if") (Constant 1)) e) -> eval e
        otherwise -> (Application func arg)
eval e = e
