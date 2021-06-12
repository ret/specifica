module Main where

import           Language.TLAPlus.Quasiquote
import           Language.TLAPlus.Syntax
import           Language.TLAPlus.Pretty
import           Language.TLAPlus.Eval

import Control.Monad (forM_)

e0 :: AS_UnitDef
e0 =
  let foo = [tla_e|LET a == 1 IN a+2 |]
  in [tla_u|ASSUME $foo |]


e1 :: AS_Spec
e1 =
  let foo = [tla_e|LET a == 1 IN a+2 |]
  in [tla_s|----
       MODULE mod  ----
       Foo(x) == <<"a", x>>
       Fox[x \in 1..100] == <<"b", x>>
       ASSUME Foo($foo)
       ASSUME Fox[43]
       ====
     |]

main = do
  putStrLn $ show e0
  ---
  let spec = e1
  case eval [spec] (CFG_Config Nothing []) of
    Left err ->
      putStrLn $ ppError err
    Right l -> do
      forM_ l $ \v -> do
        putStrLn $ prettyPrintVA v
