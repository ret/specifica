# Specifica

Specifica is a collection of TLA+ related Haskell libraries comprising a tlaplus parser, pretty printer, and expression evaluator. The code here is meant to serve as a starting point for developers who wish to build small TLA+ related utilities quickly.

## Installation

Follow these steps to install and invoke a simple sample application called `tle` to evaluate a TLA+ expression from stdin.

1. Install the [Haskell tool stack utility](https://docs.haskellstack.org/en/stable/README/#how-to-install)
2. add `~/.local/bin/` to your path (this is where `stack` installs binaries

3. clone this repo with `git clone https://github.com/ret/specifica`
4. `cd specifica`
5. `stack install`

The [source for the tle sample](https://github.com/ret/specifica/blob/master/tlaplus-eval/src/tle.hs) is just 32 lines of code. Check it out!

## A Few Examples

These steps will build the `tle` utility and copy it to `~/.local/bin/tle`. Now, we're ready to try out `tle` like so:

`$ echo 'LET a == 1 b == {a} \cup {42} IN [x \in b |-> SUBSET {x}]' | tle`

which prints the following to the terminal:

```
** INPUT (pretty-printed):
LET a ==
      1
    b ==
      {a} \cup {42}
 IN [x \in b |-> SUBSET ({x})]

** RESULT:
[1 |-> {{},{1}},
 42 |-> {{},{42}}]
```

### Define and Evaluate a Function

Here's another simple example. This time involving a function:

```
$ echo 'LET Nat == 0..3 factorial[n \in Nat] == IF n = 0 THEN 1 ELSE n * factorial[n-1] IN factorial[3]' | tle
```

and `tle` prints:

```
** INPUT (pretty-printed):
LET Nat ==
      0..3
    factorial[n \in Nat] == IF n = 0 THEN 1 ELSE n * factorial[n-1]
 IN factorial[3]

** RESULT:
6
```

### When Things Go Wrong ...

This is also a good example to show the type of runtime errors we might encounter during evaluation. Let's say we don't get the `Nat` set quite right and leave out 0. So, instead of `Nat == 0..3`, we use `Nat == 1..3`:

```
$ echo 'LET Nat == 1..3 factorial[n \in Nat] == IF n = 0 THEN 1 ELSE n * factorial[n-1] IN factorial[3]' | tle
```

in this case, `tle` prints the following error message:

```
** INPUT (pretty-printed):
LET Nat ==
      1..3
    factorial[n \in Nat] == IF n = 0 THEN 1 ELSE n * factorial[n-1]
 IN factorial[3]
** EVALUATION ERROR **
:1:27:
    value of (n) violated range Nat
    in expression factorial[n-1] at: :1:75
    where (n) was bound to
        0
    in context
        n ==> 1
        n ==> 2
        n ==> 3
        factorial ==> factorial[n \in Nat] == IF n = 0
                                              THEN 1
                                              ELSE n * factorial[n-1]
        Nat ==> Nat ==
          1..3
```

We can see that the evaluator counted down from 3,2,1, but ultimately the expression `n \in Nat` in the function's domain failed since 0 wasn't in the `Nat` set.

### Speed Bumps (the evaluator is very naive about powersets)

The following expression calculates the sum of the numbers 1,2,3:

```
$ echo 'LET Nat == 1..3 sum[ss \in SUBSET Nat] == IF ss = {} THEN 0 ELSE LET p == CHOOSE any \in ss: TRUE IN p + sum[ss \ {p}] IN sum[1..3]' | tle
```

and yields the expected 6:

```
** INPUT (pretty-printed):
LET Nat ==
      1..3
    sum[ss \in SUBSET (Nat)] == IF ss = {}
                                THEN 0
                                ELSE LET p ==
                                           CHOOSE any \in ss: TRUE
                                      IN p+sum[ss \ {p}]
 IN sum[1..3]

** RESULT:
6
```

Our evaluator is super simple and re-computes the powerset (`SUBSET`) in each recursion to check that `ss \in SUBSET Nat` holds. Because the size of the powerset grows `O(2^n)`, picking a larger `Nat` set will slow down `tle` rapidly!

### More Examples - Cross Products

Lastly, here's a fun example showing a cross product:

```
$ echo 'LET S==1..3 IN S \X S' | tle
```

resulting in the following output:

```
** INPUT (pretty-printed):
LET S ==
      1..3
 IN S \X S

** RESULT:
<<{1,2,3},{1,2,3}>>
```
