# Specifica [![Build Status](https://github.com/ret/specifica/workflows/Haskell%20Stack%20Project%20CI/badge.svg)](https://github.com/ret/specifica/actions)

Specifica is a collection of TLA+ related Haskell libraries comprising a tlaplus parser, pretty printer, and expression evaluator. The code here is meant to serve as a starting point for developers who wish to build small TLA+ related utilities quickly.

## Requirements

The project builds with the [Haskell tool Stack](https://docs.haskellstack.org/) and is tested
against Stackage **LTS 24.60 (GHC 9.10.3)**. Stack downloads the matching GHC automatically, so
you do not need a system-wide GHC — only a C toolchain and a few native libraries. On
Debian/Ubuntu: `sudo apt-get install build-essential libgmp-dev libtinfo-dev zlib1g-dev`.

## Installation

Follow these steps to install and invoke a simple sample application called `tle`, which evaluates
a TLA+ expression read from stdin.

1. Install Stack. The easiest way (macOS and Linux alike) is via [GHCup](https://www.haskell.org/ghcup/):
   ```
   curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
   ```
   then `ghcup install stack --set`.

2. Put the toolchain and the stack-installed binaries on your `PATH` by adding these lines to
   `~/.bashrc`:
   ```
   [ -f "$HOME/.ghcup/env" ] && . "$HOME/.ghcup/env"   # ghc, cabal, stack
   export PATH="$HOME/.local/bin:$PATH"                 # tle, tli (stack install target)
   ```
   Then open a new terminal, or run `source ~/.bashrc`, so `stack` and the tools are found.
   (The GHCup installer usually offers to add the first line for you.)

3. Clone and build:
   ```
   git clone https://github.com/ret/specifica
   cd specifica
   stack install
   ```
   `stack install` copies the `tle` and `tli` executables to `~/.local/bin/`.

The [source for the tle sample](https://github.com/ret/specifica/blob/master/tlaplus-eval/app/tle.hs) is just 32 lines of code. Check it out!

## Tests

Invoke `stack test` to run the test-suite (see [tlaplus-tests](https://github.com/ret/specifica/tree/main/tlaplus-tests)).

## A Few Commandline Examples

With `tle` on your `PATH` (see above), try it like so:

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
(1 :> {{},{1}} @@
 42 :> {{},{42}})
```

A concrete function value is printed with the TLC-module operators `:>` (a one-entry function)
and `@@` (function merge), the same notation the TLA+ tools' TLC model checker uses. The result
is therefore itself valid, re-parseable TLA+ — you can pipe it straight back into `tle`.

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

Our evaluator is super simple and re-computes the powerset (`SUBSET`) in each recursion to check that `ss \in SUBSET Nat` holds. Because `tle` evaluates eagerly and the size of the powerset grows with `O(2^n)`, picking a larger `Nat` set will slow down `tle` rapidly!

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
{<<1,1>>,
 <<1,2>>,
 <<1,3>>,
 <<2,1>>,
 <<2,2>>,
 <<2,3>>,
 <<3,1>>,
 <<3,2>>,
 <<3,3>>}
```

## Quasiquoter (Haskell specific)

We use Quasiquoters to embed TLA+ expressions and specifications in Haskell code. The test suite uses this feature heavily and may serve as an example to build on for other tool developers.
