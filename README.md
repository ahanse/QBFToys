# QBF and DQBF to TPTP THF converters 

This repository contains tools to convert QBF problems in QDIMACs format
and DQBF problem in BUNSAT format to higher-order problems in TPTP THF
format.

## Building the Tool
Both tools are written in Haskell. While there are multiple ways to build the
tools, using the [Stack](https://haskellstack.org) build toolis strongly
recommended by us and documented here. To install Stack follow the
nstructions on the Stack homepage.

The `qbfTool` folder contains the source code for the QBF to
HOL converter, and the DQBF tool is in the `dqbfTool` folder.
After the installation of Stack is complete, open a terminal and navigate to
either folder. Then run then following commands, where `{tool}` is
either `qbfTool`, or `dqbfTool`:

```bash
> stack setup
> stack build
> stack exec {tool}
```

The `stack build` command outputs the path the resulting binary is
placed in. On Linux calling `stack install` will copy the
binary to `~/.local/bin`. Alternatively, the programs can be run
with additional arguments (`{args}`) by calling

```bash
> stack exec {tool} -- {args}
```

## Converting QBF Problems

The QDIMACS format is widely used to represent QBF problems in CNF. It is an
extension of the DIMACS format used for propositional problems and QDIMACS is
compatible with DIMACS. Every valid DIMACS file is also a valid QDIMACS file.
Variables in the QDIMACS format are expressed by non-zero natural numbers and
literals are expressed by non-zero integers. The variable index of a literal
is the absolute value of the integer, the polarity is represented by the sign
of the integer. The following is a small QDIMACS file:

```
p cnf 3 3
c an example problem
a 1 2
e 3
-1 -3 -2 0
-2 -3 -1 0
3 0
```

A formal grammar of the QDIMACS format can be found
[online](http://www.qbflib.org/qdimacs.html). This grammar forbids empty
clauses and files without any clauses. We found that BLOQQER sometimes does
generate problems with empty clauses and completely empty problems. Hence,
our tool tries to be very liberal when parsing QDIMACS problems. It
especially ignores the metadata given in the first line.

The default behavior of `qbfTool` is to accept a QDIMACS problem
on the standard input and to output a TPTP problem to standard output.
Assuming a file `test.qdimacs` which contains the following
QDIMACS problem:

```
p cnf 6 3
e 1 0
a 0
e 2 0
a 3 4 0
e 5 6 0
1 3 5 0
1 2 0
2 4 6 0
99 0
```

This problem is not a valid QDIMACS file. The meta values are wrong and a
quantifier line contains an empty list of variables. Executing `qbfTool`
on this problem results in the following output:

```
> cat test.qdimacs | stack exec qbfTool
thf(c,conjecture,(? [X1: $o,X2: $o,X3: $o]:(! [X4: $o,X5: $o]:(? [X6: $o,X7: $o]:((X1|X4|X6)&(X1|X3)&(X3|X5|X7)&(X2)))))).
```

This output is not a direct translation of the input problem. The `qbfTool`
also applies some normalization steps. Those are:

- Consequitive lines containing the same quantifier are merged.
- Empty quantifiers are removed and the surrounding quantifiers are merged.
- Variables are renamed by the order they appear in the quantifier. This also
   assures that all variables are used without gap.
- Variables which are used in the matrix, but which do not appear in the
quantifier prefix, are added to the outermost quantifier if the outermost
quantifier is an existential quantifier. Otherwise, an existential quantifier
containing those variables is added.
- If the input problem does not contain any clauses, the clause
  `x∨¬x` is added. 
- If the input problem contains the
empty clause, a clause `x` and a clause `¬x` is added.

If the tool is called with the command line parameter `-n`, the output is a
valid QDIMACS problem resulting from these transformations instead of a TPTP
problem. For the QDIMACS problem given above this output is:

```
p cnf 7 4
e 1 2 3 0
a 4 5 0
e 6 7 0
1 4 6 0
1 3 0
3 5 7 0
2 0
```

As described above, free variables are *implicitly* existentially quantified
in the input problem and *explicitly* existentially quantified in the output
problem. Hence, the input QBF problem is satisfiable if the output problem is
valid. A resolution prover will negate the input problem and show its
unsatisfiability. Negating a problem in clause normal form and clausifying
the negated problem requires repeated application of the distributive laws
and is therefore an expensive operation. Furthermore, a prover is geared
towards showing validity and will seldom deduce that an input problem is not
valid. Hence, the `-i` command line parameter will instruct `qbfTool`
to add a negation in front of the output problem. The resulting problem will
be a theorem if an only if the input problem is unsatisfiable.

Finally, the `-o {filename}` parameter can be used to instruct `qbfTool` to
write the output into a file. If this parameter is used, `qbfTool` will
output exactly one line to the standard output. This line will contain the
number of variables and number of clause in the problem separated by a space.

## Converting DQBF Problems

The `dqbfTool` generates HOL problems in the TPTP format from DQBF problems
in the BUNSAT format. This format supports input problems which are not in
CNF.

The following example of the BUNSAT format was taken from the [BUNSAT web
page](https://www.react.uni-saarland.de/tools/bunsat/). This page also
contains a grammar of the format.

```
A x1 x2: E{x1} y1 : E{x2} y2: ((~y1) | y2) <--> (x1 ^ x2)
```

The `dqbfTool` behaves the same way as the `qbfTool` behaves, but does not
apply any normalizations to the input problem. Hence, the `-n` command line
parameter is not supported. The `-i` parameter adds a negation symbol to the
output and the `-o` parameter writes the output into a file. If the `-o`
parameter is given, the tool does not write any metadata to the standard
output.