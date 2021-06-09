module Main where

import           Language.TLAPlus.Quasiquote
import           Language.TLAPlus.Syntax
import           Language.TLAPlus.Pretty
import           Language.TLAPlus.Eval

e0 :: (EnvExpr, AS_Expression)
e0 =
  let servers =     [tla|{"A","B"} |]
      serverState = [tla|[A |-> "ready",
                          B |-> "ready"] |]
   in ([ ("Servers",     servers),
         ("serverState", serverState)
       ],
       [tla|\A server \in Servers:
              serverState[server] \in {"ready"}
       |])
-- TRUE


e1 :: (EnvExpr, AS_Expression)
e1 = ([],
      [tla|LET x == {x \in 1..3: TRUE} IN x |])
-- {1,2,3}


e2 :: (EnvExpr, AS_Expression)
e2 =
  ([],
   [tla|LET a == {2*x : x \in 1 .. 3}
            b == 1
         IN /\ a \subseteq 2..6
            /\ \E z \in 1..3: z = b
   |])
-- TRUE


e3 :: (EnvExpr, AS_Expression)
e3 =
  ([],
   [tla|LET Nat == 0..100
            factorial[n \in Nat] ==
              IF n = 0
              THEN 1
              ELSE n * factorial[n-1]
         IN factorial[4]
   |])
-- 24


main = do
  let (benv, expr) = e0
  case mkEnv benv of
    Left err ->
      putStrLn $ "Failed to evaluate env values: " ++ show err
    Right env -> do
      putStrLn $ ppEnv env
      case evalE env expr of
        Left err ->
          putStrLn $ ppError err
        Right v ->
          putStrLn $ prettyPrintVA v


-- helpers
type EnvExpr = [(String, AS_Expression)]

mkEnv :: [(String, AS_Expression)] -> Either EvalError Env
mkEnv = mapM (\(n, e) -> do
                 v <- evalE [] e
                 return (([],n), v))
