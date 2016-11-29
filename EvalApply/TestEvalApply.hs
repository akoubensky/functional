module TestEvalApply where

test :: Expr
test = Letrec
   [("fact",
     (Lambda
         "n"
         (If
            (Application
               (Function "EQ0")
               (Variable "n"))
            (Integral 1)
            (Application
               (Application
                  (Function "MULT")
                  (Variable "n"))
               (Application
                  (Variable "fact")
                  (Application
                     (Function "PRED")
                     (Variable "n"))))
         )))]
   (Application (Variable "fact") (Integral 6))
