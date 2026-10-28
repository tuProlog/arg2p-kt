---
title: Python
weight: 36
---

## Python API

The Python package `arg2p` (in `python/` in the repository) runs the library of the JVM through
[JPype](https://jpype.readthedocs.io): every profile, semantics and graph extension is there, and so is the
[parallel solver]({{% ref "/docs/modules/parallel-solver" %}}). A JVM (Java 17 or later) is needed.

### Installing

The package carries the jars of the JVM library, which Gradle copies into it:

```sh
./gradlew :parallel-solver:pythonJars
pip install ./python
```

`ARG2P_CLASSPATH` (paths separated as in `PATH`) points the package at other jars.

### Answering queries

```python
from arg2p import Flags, Solver

solver = Solver("r1 : [] => a.\nr2 : [] => -a.\nsup(r1, r2).\n")
solver.answer("a")        # Answer(in_=['a'], out=[], und=[])
```

`Flags` sets the [flags]({{% ref "/docs/api/flags" %}}) of `FlagsBuilder` in snake case (`graph_build_mode`,
`argument_labelling_mode`, `graph_extensions`, ...). A flag left out keeps its default, or the value the
[profile]({{% ref "/docs/profiles" %}}) of the theory presets. A theory its profile refuses raises `Arg2pError`, with
the reason.

| Python | Does |
| --- | --- |
| `Solver(theory, flags)` | The solver of a theory |
| `solver.answer(query)` | The statements for the query labelled in, out and und (`answerQuery/4`) |
| `solver.graphs()` | The graph of each labelling: arguments, labels, attacks, supports |
| `solver.solve(goal, limit)` | The bindings of each solution of a Prolog goal, as Python values |
| `solver.add(clauses)` | Adds clauses to the theory |
| `evaluate(theory, flags)`, `answer_query(theory, query, flags)` | The same, for a theory evaluated once |
| `Cluster(threads, parts, flags, lends)` | A cluster of the parallel solver: `load`, `add`, `solve`, `listen`, `connect`, `discover` |

Prolog terms come back as Python values: atoms as strings, numbers as numbers, lists as lists, compound terms as
`Struct` (written as Prolog writes them by `str`) and variables as `Var`.

### From the command line

```sh
arg2p theory.pl                    # the statements of each labelling, by label
arg2p theory.pl -q a --query=-a    # the labels of some statements
arg2p theory.pl -p aba -s preferred
```
