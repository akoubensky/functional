module Compiler where

import Data.List(findIndex)

-- �������� ��� ������ ��� ������������� ��������� 
-- ������������ ������-���������� � "������" SECD-������
data Expr =
    Integral Integer            | -- ����� ���������
    Logical Bool                | -- ���������� ���������
    Nil                         | -- ��������� - ������ ������
    Function String             | -- ���������� �������
    Variable String             | -- ����������
    Lambda   String Expr        | -- ������-���������
    Application Expr Expr       | -- ���������� �������
    If Expr Expr Expr           | -- �������� ���������
    Let String Expr Expr        | -- ������������� ����
    Letrec [(String, Expr)] Expr  -- ����������� ����
             deriving (Show, Eq)
data Command =
    Load (Int, Int)    | -- �������� �������� �� �������� ����
    LoadConst WHNF     | -- �������� ���������
    LoadFunc [Command] | -- �������� ���������
    LoadSect String    | -- �������� ���������� ��������
    Select [Command] [Command] |  -- ����� ������������
    Apply              | -- ���������� �������
    Return             | -- ������� �� �������
    LetApply           | -- ���� � ������������� ����
    Dummy              | -- ����������� ���������� ���������
    RecApply           | -- ���� � ����������� ����
    Stop                 -- ��������� ������
             deriving (Show, Eq)

-- "���������� ����������"
data WHNF =
    Numeric Integer            |
    Boolean Bool               |
    List [WHNF]                |
    Closure [Command] Context  |
    Section String Int [WHNF]
            deriving (Show, Eq)

-- ����������� ����� �������� � ��������������� �������
type Context = [[String]]

---- ����������:
compile    :: Expr -> [Command]
(***)      :: Expr -> Context -> [Command]
comp       :: [[String]] -> Expr -> [Command] -> [Command]
compile = (*** [])
e *** context = comp context e [Stop]

-- ���������� �������� ���� ���������� � �������� ��������� ����
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
