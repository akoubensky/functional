{-
  Анализатор для простого языка, прдставленного регулярным выражением:
  digit = 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9
  unsigned = digit+
  integral = [+ | -] unsigned
  number = integral [. unsigned] [e integral]
-}
module Number where

-- Язык - это множество слов, представленное характеристической функцией
type Language = String -> Bool

-- Формирование языка из единственного односимвольного слова
symbol   ::  Char -> Language
symbol c word  =  [c] == word

-- Альтернация языков
alt      ::  Language -> Language -> Language
(lang1 `alt` lang2) word = (lang1 word) || (lang2 word)

-- Катенация языков
cat      ::  Language -> Language -> Language
(lang1 `cat` lang2) []         = (lang1 []) && (lang2 [])
(lang1 `cat` lang2) word@(x:s) = (lang1 [] && lang2 word) ||
               (lang1' `cat` lang2) s where lang1' w = lang1 (x:w)

-- Добавление слова в язык
(<+>)   ::  Language -> String -> Language
(lang <+> word) w  =  (w == word) || (lang w)

-- Удаление слова из языка
(<->)   ::  Language -> String -> Language
(lang <-> word) w  =  (w /= word) && (lang w)

-- Итерация языка
iter   ::  Language -> Language      -- iter exp  ~  exp*
iter lang []  =  True
iter lang w   =  (lang' w) || ((lang' `cat` (iter lang')) w)
                 where lang' =  lang <-> []

-- Добавление пустого слова в язык (конструкция [language])
poss   ::  Language -> Language      -- poss exp  ~  [exp]
poss lang = lang <+> []

-- Определение конкретного языка

-- Цифра
digit = symbol '0' `alt` symbol '1' `alt` symbol '2' `alt` 
        symbol '3' `alt` symbol '4' `alt` symbol '5' `alt` 
        symbol '6' `alt` symbol '7' `alt` symbol '8' `alt` symbol '9'

-- Беззнаковое целое
unsigned = digit `cat` (iter digit)

-- Целое (возможно, со знаком)
integral = poss ((symbol '+') `alt` (symbol '-')) `cat` unsigned 

-- Число
number   = integral `cat` poss (symbol '.' `cat` unsigned)
                    `cat` poss (symbol 'e' `cat` integral)

-- Тесты
testTrue = (number "+123.123", number "12.12e-2", number "0")
testFalse = (number "", number "123.123.123", number "-12.12e--2")                    