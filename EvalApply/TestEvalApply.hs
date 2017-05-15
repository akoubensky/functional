module TestEvalApply where
 
import EvalApply(Expr(..), interpret)

test :: Expr
-- let fact = \n.if (EQ0 n) 1 (MULT n (fact (PRED n))) in fact 6
-- interpret test --> 120
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
