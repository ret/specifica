module Main where

import           Language.TLAPlus.Quasiquote
import           Language.TLAPlus.Syntax
import           Language.TLAPlus.Pretty
import           Language.TLAPlus.Eval

import Control.Monad (forM_)

e0 :: AS_UnitDef
e0 =
  let foo = [tla_e|LET a == 1 IN a+2 |]
  in [tla_u|EVAL $foo |]


main = do
  putStrLn $ show e0
