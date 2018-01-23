# Specifica

Specifica is a collection of TLA+ related Haskell libraries comprising a tlaplus parser, pretty printer, and expression evaluator. The code here is meant to serve as a starting point for developers who wish to build small TLA+ related utilities quickly.

# Getting Started

Follow these steps to install and invoke a simple sample application called `tle` to evaluate a TLA+ expression from stdin.

1. Install the [Haskell tool stack utility](https://docs.haskellstack.org/en/stable/README/#how-to-install)
2. add `~/.local/bin/` to your path (this is where `stack` installs binaries

3. clone this repo with `git clone https://github.com/ret/specifica`
4. `cd specifica`
5. `stack install`

These steps will build the `tle` utility and copy it to `~/.local/bin/tle`. Now, we're ready to try out `tle` like so:

`echo 'LET a == 1 b == {a} \cup {42} IN [x \in b |-> SUBSET {x}]' | tle`

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
