module Log(Log,logf,logv,getLog,getContent) where

-- Сохранение журнала действий
data Log a = Log String a deriving Show

-- Создание начального состояния журнала
logv :: String -> a -> Log a
logv msg x = Log (msg ++ "\n") x

-- Функция преобразования значения превращается в функцию,
-- которая дополнительно сохраняет сообщение в журнале.
logf :: String -> (a -> b) -> (a -> Log b)
logf msg f x = Log (msg ++ "\n") (f x)

-- Функция доступа к журналу
getLog :: Log a -> String
getLog (Log s _) = s

-- Функция извлечения значения
getContent :: Log a -> a
getContent (Log _ x) = x

-- Определение монадических свойств журнала

-- Функтор
instance Functor Log where
  fmap f (Log s x) = Log s (f x)

-- Аппликативный функтор
instance Applicative Log where
  pure = Log ""
  (Log s f) <*> (Log s1 x) = Log (s ++ s1) (f x)

-- Монада
instance Monad Log where
  return = pure
  (Log s x) >>= f = case f x of { Log m y -> Log (s ++ m) y }
