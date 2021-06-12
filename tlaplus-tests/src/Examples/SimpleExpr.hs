module Main where

import           Language.TLAPlus.Quasiquote
import           Language.TLAPlus.Syntax
import           Language.TLAPlus.Pretty
import           Language.TLAPlus.Eval

import Control.Monad (forM_)

e0 :: (EnvExpr, AS_Expression)
e0 =
  let servers =     [tla_v|{"A","B"} |]
      serverState = [tla_v|[A |-> "ready",
                            B |-> "ready"] |]
   in ([ (mkName "Servers",     servers),
         (mkName "serverState", serverState)
       ],
       [tla_e|\A server \in Servers:
                serverState[server] \in {"ready"}
       |])
-- TRUE


e1 :: (EnvExpr, AS_Expression)
e1 = ([],
      [tla_e|LET x == {x \in 1..3: TRUE} IN x |])
-- {1,2,3}


e2 :: (EnvExpr, AS_Expression)
e2 =
  ([],
   [tla_e|LET a == {2*x : x \in 1 .. 3}
              b == 1
           IN /\ a \subseteq 2..6
              /\ \E z \in 1..3: z = b
   |])
-- TRUE


e3 :: (EnvExpr, AS_Expression)
e3 =
  ([],
   [tla_e|LET Nat == 0..100
              factorial[n \in Nat] ==
                IF n = 0
                  THEN 1
                  ELSE n * factorial[n-1]
           IN factorial[4]
   |])
-- 24


e4 :: (EnvExpr, AS_Expression)
e4 =
  let x = [tla_e|LET foo==41 IN foo|]
  in ([], [tla_e|1+$x|])
-- 42


e5 :: (EnvExpr, AS_UnitDef)
e5 =
  let foo = [tla_e|LET a == 1 IN a+2 |]
  in ([], [tla_u|ASSUME $foo |])


e6 :: (EnvExpr, AS_Spec)
e6 =
  let foo = [tla_e|LET a == 1 IN a+2 |]
  in ([], [tla_s|----
     MODULE mod  ----
     Foo(x) == <<"a", x>>
     Fox[x \in 1..100] == <<"b", x>>
     ASSUME Foo($foo)
     ASSUME Fox[43]
     ====
     |])


main = do
  putStrLn $ show e6
  let (env, spec) = e6
  case eval [spec] (CFG_Config Nothing []) of
    Left err ->
      putStrLn $ ppError err
    Right l -> do
      forM_ l $ \v -> do
        putStrLn $ prettyPrintVA v


main2 = do
  let (env, expr) = e4
  putStrLn $ ppEnv env
  case evalE env expr of
    Left err ->
      putStrLn $ ppError err
    Right v ->
      putStrLn $ prettyPrintVA v


-- helpers
type EnvExpr = [( ([String],String), VA_Value)]
mkName n = ([], n)
