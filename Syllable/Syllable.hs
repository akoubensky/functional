{-
  Анализатор для простого языка, прдставленного регулярным выражением:
  vowel     = a | o
  consonant = c | l | k | b
  letter    = vowel | consonant
  open      = consonant vowel | consonant vowel vowel
  closed    = open consonant
  syllable  = open | closed
-}
module Syllable where

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

-- Определение конкретного языка

-- Гласная
vowel     = (symbol 'a') `alt` (symbol 'o')

-- Согласная
consonant = (symbol 'c') `alt` (symbol 'l') `alt`
            (symbol 'k') `alt` (symbol 'b')

-- Буква (гласная или согласная)
letter    = vowel `alt` consonant

-- Открытый слог
open      = (consonant `cat` vowel) `alt` 
            ((consonant `cat` vowel) `cat` vowel)

-- Закрытый слог
closed    = open `cat` consonant

-- Слог
syllable  = open `alt` closed

-- Тесты
testTrue = (syllable "cool", syllable "cab", syllable "cook")
testFalse = (syllable "cat", syllable "cock", syllable "cocacola")