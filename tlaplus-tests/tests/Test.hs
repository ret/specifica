import           Language.TLAPlus.Eval(evalENoFail, evalSpecNoFail)
import           Language.TLAPlus.Quasiquote(tla_v, tla_e, tla_s)

import           Test.Tasty (defaultMain, TestTree, testGroup)
import           Test.Tasty.HUnit

main = defaultMain tests

tests = testGroup "All" [
    testGroup "Basic" [setTests, operatorTests]
  , testGroup "Environment (eval)" [bindingTests]
  , testGroup "Evaluation in specification context" [specEvalTests]
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
        
    -- from: http://lamport.azurewebsites.net/pubs/teaching-concurrency.pdf
  , testCase "nested LETs" $
      [tla_v|LET
        GCD (m,n) == 
          LET DivisorsOf(p) == {d \in 1..p: \E q \in 1..p: p = d*q}
              MaxElementOf(S) == CHOOSE s \in S: \A t \in S: s >= t
           IN MaxElementOf(DivisorsOf(m) \cap DivisorsOf(n))
        IN GCD(12,18)
      |]
      @?=
      [tla_v|6|]

  ]
  where
    mkName n = ([], n)


specEvalTests :: TestTree
specEvalTests = testGroup "Spec evaluation"
  [ testCase "basic spec eval" $
    let foo = [tla_e|LET a == 1 IN a+2 |]
     in evalSpecNoFail [tla_s|----
          MODULE SomeName  ----
          Foo(x) == <<"a", x>>
          Fox[x \in 1..100] == <<"b", x>>
          EVAL Foo($foo) \* in Specifica EVAL evaluates and yields expr value
          EVAL Fox[42]   \* in Specifica EVAL evaluates and yields expr value
          ====
        |]
    @?=
    [ [tla_v|<<"a",  3>> |]
    , [tla_v|<<"b", 42>> |] ]
    
    -- from: http://lamport.azurewebsites.net/pubs/teaching-concurrency.pdf
  , testCase "GCD example" $
      evalSpecNoFail [tla_s|----
        MODULE GCDTest ----
        GCD (m,n) == 
          LET DivisorsOf(p) == {d \in 1..p: \E q \in 1..p: p = d*q}
              MaxElementOf(S) == CHOOSE s \in S: \A t \in S: s >= t
           IN MaxElementOf(DivisorsOf(m) \cap DivisorsOf(n))
        EVAL GCD(12,18)
        ====
      |]
      @?=
      [ [tla_v|6|] ]
  ]

spliceTests :: TestTree
spliceTests = testGroup "Splice expression"
  [ testCase "splice expr into expr" $
      let x = [tla_e|LET foo==41 IN foo|]
       in evalENoFail [] [tla_e|1+$x|]
      @?=
      [tla_v|42|]
        ,  testCase "pattern-match using expression splice" $
       -- note x is on the LHS (pattern to match) and will be bound to 1.
       let [tla_e|$x+2|] = [tla_e|1+2|] 
        in x @?= [tla_e|1|]
  ]
