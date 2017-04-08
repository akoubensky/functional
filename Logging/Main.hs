-- Тестирование функций работы с журналом
module Main where

import Log

-- Аналог fmap с переставленными аргументами
(<&>) :: Functor m => m a -> (a -> b) -> m b
(<&>) = flip fmap
infixl 1 <&>

test = logv "Start with 5" 5 
       >>= logf "Added 7" (+7)
       <&> (subtract 10)
       >>= logf "Added 2" (+2)
       >>= logf "Replcate 10" (replicate 10)
       >>= logf "Add naturals" (zipWith (+) [1..])
       >>= logf "Squared" (map (\x -> x*x))

main = do
  putStr $ getLog test
  print $ getContent test
