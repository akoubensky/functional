module Compiler where

import Data.List(findIndex)

-- Основной тип данных для представления выражений 
-- расширенного лямбда-исчисления и "команд" SECD-машины
data Expr =
    Integral Integer            | -- целая константа
    Logical Bool                | -- логическая константа
    Nil                         | -- константа - пустой список
    Function String             | -- встроенная функция
    Variable String             | -- переменная
    Lambda   String Expr        | -- лямбда-выражение
    Application Expr Expr       | -- применение функции
    If Expr Expr Expr           | -- условное выражение
    Let String Expr Expr        | -- нерекурсивный блок
    Letrec [(String, Expr)] Expr  -- рекурсивный блок
             deriving (Show, Eq)
data Command =
    Load (Int, Int)    | -- загрузка значения по адресной паре
    LoadConst WHNF     | -- загрузка константы
    LoadFunc [Command] | -- загрузка замыкания
    LoadSect String    | -- загрузка встроенной операции
    Select [Command] [Command] |  -- выбор альтернативы
    Apply              | -- применение функции
    Return             | -- возврат из функции
    LetApply           | -- вход в нерекурсивный блок
    Dummy              | -- образование фиктивного контекста
    RecApply           | -- вход в рекурсивный блок
    Stop                 -- остановка работы
             deriving (Show, Eq)

-- "результаты вычислений"
data WHNF =
    Numeric Integer            |
    Boolean Bool               |
    List [WHNF]                |
    Closure [Command] Context  |
    Section String Int [WHNF]
            deriving (Show, Eq)

-- Определения типов основных и вспомогательных функций
type Context = [[String]]

---- Компилятор:
compile    :: Expr -> [Command]
(***)      :: Expr -> Context -> [Command]
comp       :: [[String]] -> Expr -> [Command] -> [Command]
compile = (*** [])
e *** context = comp context e [Stop]

-- Вычисление адресной пары переменной в заданном контексте имен
addr :: Eq a => a -> [[a]] -> (Int, Int)
addr var (vars:rest) = case findIndex (== var) vars of
        Nothing -> (lev+1, num)
        Just idx -> (0, idx)
    where (lev, num) = addr var rest

comp env (Integral n) lc = (LoadConst (Numeric n)) : lc
comp env (Logical b) lc = (LoadConst (Boolean b)) : lc
comp env Nil lc = (LoadConst (List [])) : lc
comp env (Function f) lc = (LoadSect f) : lc
comp env (Variable x) lc = (Load (addr x env)) : lc
comp env (Lambda var e) lc =
         (LoadFunc (comp ([var]:env) e [Return])) : lc
comp env (Application func arg) lc =
         ((comp env arg) . (comp env func)) (Apply:lc)
comp env (If cond th el) lc =
         comp env cond
              ((Select (comp env th []) (comp env el [])):lc)
comp env (Let x e body) lc =
    comp env e
        ((LoadFunc (comp ([x]:env) body [Return])) : Apply : lc)
comp env (Letrec pairs body) lc =
     Dummy : (LoadConst (List [])) :
     (foldr (comp' env)
         ((LoadFunc (comp (names:env) body [Return])):
                     RecApply:lc) (reverse values))
  where (names, values) = unzip pairs
        comp' e ex lc = comp e ex ((LoadSect "cons"):Apply:Apply:lc)
