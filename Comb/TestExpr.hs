module TestExpr where

import ShowExpr

-- Примеры выражений
plus, true, false, cond, zero, succ', twice, expr, fact, infinite, get :: Expression

-- Функция сложения \x.\y.+ x y
plus = Lambda "x" (Lambda "y" (Application (Application (Function "+")(Variable "x"))(Variable "y")))

----------------------------
-- Чистое лямбда-исчисление
----------------------------

-- Представление логических констант TRUE = \x.\y.x (она же - вычеркиватель K) и FALSE = \x.\y.y
true = Lambda "x" (Lambda "y" (Variable "x"))
false = Lambda "x" (Lambda "y" (Variable "y"))

-- Представление функции IF = \p.\q.\r.p q r
cond = Lambda "p" (Lambda "q" (Lambda "r" (Application (Application (Variable "p")(Variable "q"))(Variable "r"))))

-- Представление нуля ZERO = FALSE = \x.\y.y
zero = false
-- Функция следования SUCC = \n.\f.\x.f (n f x)
succ' = Lambda "n" (Lambda "f" (Lambda "x" (Application (Variable "f")(Application (Application (Variable "n")(Variable "f"))(Variable "x")))))
-- Функция сложения PLUS = 
plus' = Lambda "m" (Lambda "n" (Application (Application (Variable "m") succ') (Variable "n")))

-- twice = λf.λx.f (f x)
-- expr = twice (+1) 3
twice = Lambda "f" (Lambda "x" (Application (Variable "f")(Application (Variable "f")(Variable "x"))))
expr = Application (Application twice (Application (Function "+")(Constant 1)))(Constant 3)

----------------------------
-- Рекурсивные функции
----------------------------

-- fact = \n.if (eq0 n) 1 (* n (fact (- n 1)))
fact = Lambda "n" (Application (Application (Application (Function "if")(Application (Function "eq0")(Variable "n")))(Constant 1))(Application (Application (Function "*")(Variable "n"))
       (Application (Variable "fact")(Application (Application (Function "-")(Variable "n"))(Constant 1)))))
-- построение бесконечного списка натуральных чисел
infinite = Lambda "n" (Application (Application (Function "cons")(Variable "n"))(Application (Variable "infinite")(Application (Application (Function "+")(Variable "n"))(Constant 1))))
-- индексация списка
get = Lambda "n" (Lambda "s" (Application (Application (Application (Function "if")(Application (Function "eq0")(Variable "n")))(Application (Function "head")(Variable "s")))
       (Application (Application (Function "get")(Application (Application (Function "-")(Variable "n"))(Constant 1)))(Variable "s"))))
-- yFact = Application y (Lambda "fact" fact)
-- yInf = Application y (Lambda "infinite" infinite)
-- yGet = Application y (Lambda "get" get)
-- Y-комбинатор yH = \h.(\x.h (x x)) (\x.h (x x))
h = Lambda "x" (Application (Variable "h")(Application (Variable "x")(Variable "x")))
yComb = Lambda "h" (Application h h)
