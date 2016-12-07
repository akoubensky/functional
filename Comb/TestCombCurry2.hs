module TestCombCurry2 where

import ShowExpr
import CombCurry2
import TestExpr

-- Проверяем правильность основных правил перевода
test1, test2 :: Expression
test1 = comb plus
test2 = eval (Application (Application test1 (Constant 3)) (Constant 5))
test3 = comb (Application y (Lambda "fact" fact))
test4 = eval (Application test3 (Constant 6))