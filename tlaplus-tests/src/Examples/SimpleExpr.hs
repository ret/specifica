module Main where

import           Language.TLAPlus.Quasiquote
import           Language.TLAPlus.Syntax
import           Language.TLAPlus.Pretty
import           Language.TLAPlus.Eval

e1 :: AS_Expression
e1 = [tla|LET x == {x \in 1..3: TRUE} IN x |]
-- {1,2,3}

e2 :: AS_Expression
e2 =
  [tla|LET a == {2*x : x \in 1 .. 3}
           b == 1
        IN /\ a \subseteq 2..6
           /\ \E z \in 1..3: z = b
  |]
-- TRUE

e3 :: AS_Expression
e3 =
  [tla|LET Nat == 0..100
           factorial[n \in Nat] ==
             IF n = 0
             THEN 1
             ELSE n * factorial[n-1]
        IN factorial[4]
  |]
-- 24
    
main = do
  case evalE [] e3 of
    Left err ->
      putStrLn $ ppError err
    Right v ->
      putStrLn $ prettyPrintVA v
