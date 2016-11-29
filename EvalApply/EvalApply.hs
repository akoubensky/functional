module EvalApply where

-- Основной тип данных для представления выражений
-- расширенного лямбда-исчисления
data Expr =
     Integral Integer |  -- целые константы
     Logical Bool     |  -- логические константы
     Function String  |  -- идентификаторы примитивных функций
     Variable String  |  -- переменная
     Lambda   String Expr         |  -- лямбда-выражение
     If Expr Expr Expr            |  -- условное выражение
     Application Expr Expr        |  -- применение функции
     Let String Expr Expr         |  -- простой блок
     Letrec [(String, Expr)] Expr |  -- рекурсивный блок
     Closure  String Expr Context |  -- замыкание
     Section  Int String [Expr]      -- сечение
            deriving (Show, Eq)

-- Контекст вычислений представлен ассоциативным списком
type Context = [(String, Expr)]

-- Определения типов основных и вспомогательных функций
---- Интерпретатор:
interpret  :: Expr -> Expr

---- Основные функции eval / apply
eval       :: Context -> Expr -> Expr
apply      :: Expr -> Expr -> Expr

---- Функции для применения стандартных операций
infixr 6 <+>
infixr 5 <++>
(<+>)      :: (String, Expr) -> Context -> Context
(<++>)     :: Context -> Context -> Context
assoc      :: String -> Context -> Expr
arity      :: String -> Int
intrinsic  :: String -> [Expr] -> Expr

-- Определения уравнений для вспомогательных функций
---- Поиск по контексту:
assoc x ((y, e):ctx) | x == y    = e
                     | otherwise = assoc x ctx
---- Добавление в контекст
(<+>)  = (:)
(<++>) = (++)

---- Определение числа операндов стандартных операций
arity "ADD" = 2
arity "SUB" = 2
arity "MULT" = 2
arity "DIV" = 2
arity "EQ0" = 1
arity "SUCC" = 1
arity "PRED" = 1

---- Выполнение стандартных операций над списком аргументов
intrinsic "ADD" [Integral(a), Integral(b)] = Integral (a+b)
intrinsic "SUB" [Integral(a), Integral(b)] = Integral (a-b)
intrinsic "MULT" [Integral(a), Integral(b)] = Integral (a*b)
intrinsic "DIV" [Integral(a), Integral(b)] = Integral (a `div` b)
intrinsic "EQ0" [Integral(a)] = Logical (a==0)
intrinsic "SUCC" [Integral(a)] = Integral (a+1)
intrinsic "PRED" [Integral(a)] = Integral (a-1)

-- Определения уравнений для основных функций
---- Интерпретатор:
interpret = eval []

---- eval - приведение выражения к СЗНФ в заданном контексте
eval _   e@(Integral _)  = e
eval _   e@(Logical _)   = e
eval _   (Function f)    = Section (arity f) f []
eval ctx (Lambda x e)    = Closure x e ctx
eval _ e@(Closure _ _ _) = e
eval _ e@(Section _ _ _) = e
eval ctx (Variable x)    = assoc x ctx
eval ctx (Application func arg) =
                        apply (eval ctx func) (eval ctx arg)
eval ctx (If cond t e) =
    eval ctx (if (eval ctx cond) == (Logical True) then t else e)
eval ctx (Let v a body) = eval newCtx body
    where newCtx = (v, eval ctx a) <+> ctx
eval ctx (Letrec pairs body) = eval newCtx body where
    newCtx = (map (\(v, a) -> (v, eval newCtx a)) pairs) <++> ctx

---- apply - вычисление результата применения функции к аргументу
apply (Closure x body ctx) arg = eval newCtx body
    where newCtx = (x, arg) <+> ctx
apply (Section nArgs func listArgs) arg
        | nArgs == 1 = intrinsic func newListArgs
        | otherwise  = Section (nArgs-1) func newListArgs
    where newListArgs = listArgs ++ [arg]
