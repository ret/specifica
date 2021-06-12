import           Language.TLAPlus.Eval(evalENoFail)
import           Language.TLAPlus.Quasiquote(tla_v, tla_e)

import           Test.Tasty (defaultMain, TestTree, testGroup)
import           Test.Tasty.HUnit

main = defaultMain tests

tests = testGroup "All" [
    testGroup "Basic" [setTests, operatorTests]
  , testGroup "Environment (eval)" [bindingTests]
  , testGroup "Splice (Haskell specific, meta variable handling)" [spliceTests]
  ]

setTests :: TestTree
setTests = testGroup "Set"
  [ testCase "comprehension" $
      [tla_v|LET s == {x \in 1..3: x>1}  IN s |] @?= [tla_v|{2,3}|]
  , testCase "generation" $
      [tla_v|LET s == {x*2 : x \in 1..3} IN s |] @?= [tla_v|{2,4,6}|]
  ]

operatorTests :: TestTree
operatorTests = testGroup "Operator"
  [ testCase "factorial" $
     [tla_v|LET Nat == 0..4
                factorial[n \in Nat] ==
                  IF n = 0
                    THEN 1
                    ELSE n * factorial[n-1]
            IN factorial[4]
     |]
     @?=
     [tla_v|24|]
  ]

bindingTests :: TestTree
bindingTests = testGroup "pass env to eval (basic)"
  [ testCase "simple" $
      let servers =     [tla_v|{"A","B"} |]
          serverState = [tla_v|[A |-> "ready",
                                B |-> "ready"] |]
          env = [ (mkName "Servers",     servers),
                  (mkName "serverState", serverState)]
          expr = [tla_e|\A server \in Servers:
                          serverState[server] \in {"ready"}
                       |]
       in evalENoFail env expr
      @?=
      [tla_v|TRUE|]
  ]
  where
    mkName n = ([], n)


spliceTests :: TestTree
spliceTests = testGroup "Splice expression"
  [ testCase "splice expr into expr" $
      let x = [tla_e|LET foo==41 IN foo|]
       in evalENoFail [] [tla_e|1+$x|]
      @?=
      [tla_v|42|]
  ]
