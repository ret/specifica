# Specifica

Specifica is a collection of TLA+ related Haskell libraries. It's centered around a tlaplus parser, pretty printer and expression evaluator that can serve as a starting point for a developer who wishes to build small TLA+ related utilities.

# Getting Started

Follow these steps to install and invoke a simple sample application called `tle` to evaluate a TLA+ expression from stdin.

* Install the [Haskell tool stack utility](https://docs.haskellstack.org/en/stable/README/#how-to-install)
* add `~/.local/bin/` to your path (this is where `stack` installs binaries

* clone this repo with `git clone https://github.com/ret/specifica`
* `cd specifica`
* `stack install`

This will build the `tle` utility and copy it to `~/.local/bin/tle`. Now, we're ready to try out `tle` like so:

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
