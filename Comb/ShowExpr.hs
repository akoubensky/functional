module ShowExpr where

-- Представление выражений
data Expression =
    Constant Integer |
    Function String |
    Variable String |
    Lambda String Expression |
    Application Expression Expression

-- Текстовое представление выражений с использованием приоритетов позиций конструктов
instance Show Expression where
    showsPrec _ (Constant c) = shows c
    showsPrec _ (Function f) = (f ++)
    showsPrec _ (Variable x) = (x ++)
    showsPrec p (Lambda x e) | p < 2 =
            ("\\"++) . (x ++) . ('.':) . showsPrec 1 e
                             | otherwise =
            ("(\\"++) . (x ++) . ('.':) . showsPrec 1 e . (')':)
    showsPrec 3 (Application e1 e2) =
            ('(':) . showsPrec 2 e1 . (' ':) . showsPrec 3 e2 . (')':)
    showsPrec _ (Application e1 e2) =
            showsPrec 2 e1 . (' ':) . showsPrec 3 e2
