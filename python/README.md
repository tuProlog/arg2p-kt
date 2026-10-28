# arg2p for Python

[Arg2P](https://github.com/tuProlog/arg2p-kt), structured argumentation in Prolog, from Python: the library of the
JVM, run through [JPype](https://jpype.readthedocs.io). Every profile (ASPIC+, ABA, ABA+, abstract frameworks, ...),
semantics and graph extension of the JVM library is there, and so is the parallel solver. A JVM (Java 17 or later) is
needed.

## Installing

The package carries the jars of the JVM library, copied by Gradle:

```sh
./gradlew :parallel-solver:pythonJars
pip install ./python
```

Set `ARG2P_CLASSPATH` to use other jars instead (paths separated as in `PATH`).

## Answering queries

```python
from arg2p import Flags, Solver

solver = Solver("""
r1 : [] => a.
r2 : [] => -a.
sup(r1, r2).
""")

solver.answer("a")        # Answer(in_=['a'], out=[], und=[])
solver.answer("-a").out   # [Struct(functor='-', args=('a',))], written -a
```

`Flags` sets the flags of `FlagsBuilder`, in snake case; a flag left out keeps its default, or the value the profile
of the theory presets:

```python
Solver(theory, Flags(graph_build_mode="aspic", argument_labelling_mode="preferred"))
```

A theory its profile refuses (a non-flat ABA theory, say) raises `Arg2pError`, with the reason.

## The graph

`graphs()` gives the graph of each labelling (several for a semantics such as `preferred`), as plain data:

```python
from arg2p import evaluate

(graph,) = evaluate("f1 :=> d.\nf2 :=> -d.\n")
graph.conclusions("und")                      # {'d', '-d'}
[a.descriptor for a in graph.arguments]       # ["A0 : f2 : '-'(d)", 'A1 : f1 : d']
graph.attacks                                 # (Attack(attacker='A1', target='A0', type='undermine', on='A0'), ...)
```

## Prolog goals

`solve` gives the bindings of each solution, as Python values: atoms as strings, numbers as numbers, lists as lists,
compound terms as `Struct` and variables as `Var`.

```python
solver.solve("member(X, [a, b, c])", limit=2)   # [{'X': 'a'}, {'X': 'b'}]
solver.add("f3 :=> c.")                         # more clauses
```

## The parallel solver

A `Cluster` builds the graph with workers on the threads of the JVM, and with those of the clusters it is federated
with (on the JVM, Node.js or in browsers), over WebSockets:

```python
from arg2p import Cluster

with Cluster(threads=1) as evaluating, Cluster(threads=4) as lending:
    port = evaluating.listen(0, "127.0.0.1")
    lending.connect(f"ws://127.0.0.1:{port}")   # or discover() on both, on a local network
    evaluating.load(theory)
    evaluating.solve("a")                       # Evaluation(in_=[Evaluated(claim='a', argument=...)], ...)
```

In query mode (the default) the graph is built on the rules the statements evaluated depend on.

## From the command line

```sh
arg2p theory.pl                    # the statements of each labelling, by label
arg2p theory.pl -q a --query=-a    # the labels of some statements
arg2p theory.pl -p aba -s preferred
```

## Tests

```sh
cd python
python -m venv .venv && .venv/bin/pip install -e .[test]
.venv/bin/pytest
```
