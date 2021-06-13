import           Language.TLAPlus.Eval(evalENoFail, evalSpecNoFail)
import           Language.TLAPlus.Quasiquote(tla_v, tla_e, tla_s)

import           Test.Tasty (defaultMain, TestTree, testGroup)
import           Test.Tasty.HUnit

main = defaultMain tests

tests = testGroup "All" [
    testGroup "Basic" [setTests, operatorTests, sndOrderOperatorTests,
                       recordTypeTests, functionTypeTests, sequenceTests,
                       indentationTests, crossProductTests, miscTests]
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

sndOrderOperatorTests :: TestTree
sndOrderOperatorTests = testGroup "2nd-order operator"
  [ testCase "2nd-order argument" $
     evalSpecNoFail [tla_s|----
       MODULE test ----
       Pred(x) == x > 2
       F(v,P(_)) == P(v)
       G(v) == Pred(v)
       EVAL G(3) (* OK *)
       EVAL F(2, Pred) (* OK *)
       EVAL LET x == 3 IN LET y == G(x) IN y (* OK *)
       ==== |]
     @?=
     [ [tla_v|TRUE|], [tla_v|FALSE|], [tla_v|TRUE|] ]
  , testCase "LAMBDA argument" $
     evalSpecNoFail [tla_s|----
       MODULE test ----
       F(v,P(_)) == P(v)
       EVAL F(2, LAMBDA x: x > 2)
       EVAL F(3, LAMBDA x: x > 2)
       ==== |]
     @?=
     [ [tla_v|FALSE|], [tla_v|TRUE|] ]
  , testCase "LAMBDA with operator" $
     evalSpecNoFail [tla_s|----
       MODULE test ----
       Pred(x) == x > 2
       F(v,P(_)) == P(v)
       EVAL F(2, LAMBDA x: Pred(x))
       EVAL F(3, LAMBDA x: Pred(x))
       ==== |]
     @?=
     [ [tla_v|FALSE|], [tla_v|TRUE|] ]
  , testCase "LAMBDA with param operator" $
     evalSpecNoFail [tla_s|----
       MODULE test ----
       Pred(x) == x > 2
       F(v,P(_,_)) == P(v, Pred)
       EVAL F(2, LAMBDA x, p : p(x))
       EVAL F(3, LAMBDA x, p : p(x))
       ==== |]
     @?=
     [ [tla_v|FALSE|], [tla_v|TRUE|] ]
  ]

recordTypeTests :: TestTree
recordTypeTests = testGroup "RecordType tests"
  [ testCase "enumerate records in rec type" $
     [tla_v|{x \in [a: 1..2, b: 3..4]: TRUE}|]
     @?=
     [tla_v|{[a |-> 1, b |-> 3],
             [a |-> 1, b |-> 4],
             [a |-> 2, b |-> 3],
             [a |-> 2, b |-> 4]}|]
   , testCase "union of record types" $
     [tla_v|[a: 10..11, b: 12..12] \union [c:10..11] |]
     @?=
     [tla_v|{[a: 10..11, b: 12..12], [c:10..11]} |]
  , testCase "record type conversion" $
     [tla_v|LET x == ("a":>3 @@ "b":>5) y == [a|->3, b|->5] IN x=y|]
     @?=
     [tla_v|TRUE|]
  , testCase "simple inclusion check - 1" $
     [tla_v|[a |-> 1] \in [a:1..2] |]
     @?=
     [tla_v|TRUE|]
  , testCase "simple inclusion check - 2" $
     [tla_v|"a":>1 \in [a:1..2] |]
     @?=
     [tla_v|TRUE|]
  , testCase "record type subseteq check 1" $
     [tla_v|[a:1..2] \subseteq [a:1..2] |]
     @?=
     [tla_v|TRUE|]
  , testCase "record type subseteq check 2" $
     [tla_v|[a:1..3, b:1..2] \subseteq [a:1..2, b:1..2]|]
     @?=
     [tla_v|FALSE|]
  , testCase "record type subseteq check 3" $
     [tla_v|[a:1..2, c:3..3] \subseteq [a:1..2, b:1..2]|]
     @?=
     [tla_v|FALSE|]
  ]

functionTypeTests :: TestTree
functionTypeTests = testGroup "FunctionType tests"
  [ testCase "enum function types" $
     [tla_v|{ x \in [1..3 -> 4..5]: TRUE }|]
     @?=
     [tla_v|{1:>4, 1:>5,
             2:>4, 2:>5,
             3:>4, 3:>5}|]
  , testCase "union of function types" $
     [tla_v|{x \in [3..4 -> 5..6] \union [1..2 -> 10..11 ]: TRUE}|]
     @?=
     [tla_v|{1:>5, 1:>6, 1:>10, 1:>11,
             2:>5, 2:>6, 2:>10, 2:>11,
             3:>5, 3:>6, 3:>10, 3:>11,
             4:>5, 4:>6, 4:>10, 4:>11}|]
  , testCase "nested function types" $
     [tla_v|{x \in [1..2 -> [10..11 -> 15..16] ]: TRUE}|]
     @?=
     [tla_v|{(1 :> (10 :> 15)), (1 :> (10 :> 16)), (1 :> (11 :> 15)), (1 :> (11 :> 16)),
             (2 :> (10 :> 15)), (2 :> (10 :> 16)), (2 :> (11 :> 15)), (2 :> (11 :> 16))} |]
  , testCase "record in function type - 1" $
     [tla_v|"a" :> 4 \in [{"a"} -> 3..4]|]
     @?=
     [tla_v|TRUE|]
  , testCase "record in function type - 2" $
     [tla_v|[a |-> 4] \in [{"a"} -> 3..4]|]
     @?=
     [tla_v|TRUE|]
  ]

sequenceTests :: TestTree
sequenceTests = testGroup "Sequence Tests"
  [ testCase "sequence as a function" $
      [tla_v|<<4,5>>[2]|] @?= [tla_v|5|]
  , testCase "domain of a sequence" $
      [tla_v|DOMAIN(<<4,5,6>>)|] @?= [tla_v|{1,2,3}|]
  , testCase "enumerate sequence elements" $
      [tla_v|LET s==<<4,5,6>> IN {s[i]: i \in DOMAIN(s)}|] @?= [tla_v|{4,5,6}|]
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

--  https://github.com/ret/specifica/issues/4
indentationTests :: TestTree
indentationTests = testGroup "Identation (was issue #4)"
  [ testCase "Issue #4 fixed" $
      let env = [ (mkName "y", [tla_v|FALSE|]) ]
          expr = [tla_e|LET
                   res == \/ /\ \E y \in {TRUE}: y
                             /\ y
                   IN res |]
       in evalENoFail env expr
      @?=
      [tla_v|FALSE|]
  ]

--  https://github.com/ret/specifica/issues/8
crossProductTests :: TestTree
crossProductTests = testGroup "cross-product (was issue #8)"
  [ testCase "Issue #8 fixed" $
      [tla_v|(1..2) \X (3..4)|]
      @?=
      [tla_v|{<<1,3>>, <<1,4>>, <<2,3>>, <<2,4>>}|]
  , testCase "Generate cross products (pairs)" $
      [tla_v|{<<x,y>> : x,y \in 1..2}|]
      @?=
      [tla_v|{<<1,1>>, <<1,2>>, <<2,1>>, <<2,2>>}|]
  , testCase "Generate cross products (3-tuple)" $
      [tla_v|{<<x,y,z>> : x,y,z \in 1..2}|]
      @?=
      [tla_v|{<<1,1,1>>, <<1,2,1>>, <<2,1,1>>, <<2,2,1>>,
              <<1,1,2>>, <<1,2,2>>, <<2,1,2>>, <<2,2,2>>}|]
  , testCase "Tuple generation" $
      [tla_v|{<<x+4,y-3>> : <<x,y>> \in {1,2,3} \X {1,2}}|]
      @?=
      [tla_v|{<<5,-2>>,<<5,-1>>,<<6,-2>>,<<6,-1>>,<<7,-2>>,<<7,-1>>}|]
  ]

      --  https://github.com/ret/specifica/issues/8
miscTests :: TestTree
miscTests = testGroup "Misc small items"
  [ testCase "Parse negative numbers" $
      [tla_v|-1|]
      @?=
      [tla_v|0-1|]
  ]

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

-- helpers
mkName n = ([], n)
