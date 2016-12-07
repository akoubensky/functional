module ShowExpr where

-- Представление лямбда-выражений
data Expression =
    Constant Integer |
    Function String |
    Variable String |
    Lambda String Expression |
    Application Expression Expression

-- Текстовое представление выражений
instance Show Expression where
    showsPrec _ (Constant c) = shows c
    showsPrec _ (Function f) = (f ++)
    showsPrec _ (Variable x) = (x ++)
    showsPrec _ (Lambda x e) =
        ("(\\"++) . (x ++) . ('.':) . shows e . (')':)
    showsPrec _ (Application (Application e1 e2) e3) =
        ('(':) . shows e1 . (' ':) . shows e2 . (' ':) . shows e3 . (')':)
    showsPrec _ (Application e1 e2) =
        ('(':) . shows e1 . (' ':) . shows e2 . (')':)
