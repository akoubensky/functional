import Compiler

-- Первый тест:
-- let twice = \f.\x.f (f x) in twice (+ 1) 3
test1 :: Expr
test1 = Let "twice"
  (Lambda "f" (Lambda "x"
      (Application (Variable "f")
                   (Application (Variable "f")(Variable "x")))))
  (Application
      (Application (Variable "twice")
          (Application (Function "+")(Integral 1)))(Integral 3))

-- Второй тест:
-- letrec join = \s1.\s2.if (null s1) s2 (cons (head s1) (join (tail s1) s2))
test2 :: Expr
thenPart = Variable "s2"
elsePart = Application (Application (Function "cons")
    (Application (Function "head")(Variable "s1")))
    (Application (Application (Function "join")
    (Application (Function "tail")(Variable "s1")))(Variable "s2"))

body = If (Application (Function "null")(Variable "s1"))
          thenPart elsePart

test2 = Letrec [("join", Lambda "s1" (Lambda "s2" body))]
                (Variable "join")