---
title: Python
weight: 36
---

## Python API

The Python package `arg2p` is the [API of arg2p]({{% ref "/docs/api/kotlin" %}}) in snake case: a solver for a
theory (`Arg2p`), its flags (`Flags`, with an enumeration for the values of each), its answers, graphs and
solutions, the causality tests, and the clusters of the [parallel solver]({{% ref "/docs/modules/parallel-solver" %}}).
It runs the library of the JVM through [JPype](https://jpype.readthedocs.io), on the Java runtime its wheel bundles:
no Java is needed.

```sh
pip install arg2p
```

The wheels of Windows (x86-64), Linux (x86-64, ARM64) and macOS (ARM64, x86-64) bundle a Java runtime; on another
platform the package runs on the Java of the system (21 or later, `JAVA_HOME`). `ARG2P_JVM` names another library of
the JVM to run on (`arg2p mcp` runs the `java` of its runtime), `ARG2P_CLASSPATH` other jars, put before those of the
package: their classes are the ones loaded, and the package gives those they lack.

The JVM starts at the first use of the package, once per process, with the stack of `ARG2P_STACK` (`512m`) and the
options of `ARG2P_JVM_OPTIONS`, split as a shell splits them (an option with blanks quoted, as
`"-Dlog=C:/My logs/arg2p.log"`; a backslash escapes a character, but on Windows): its heap is the default of the JVM
(a quarter of the memory), and a large graph may need more, as `ARG2P_JVM_OPTIONS="-Xmx8g"`. Set them before the
first call into the package.

---

### A solver

```python
from arg2p import Arg2p, Flags, Semantics

arg2p = Arg2p.of("""
r1 : [] => a.
r2 : [] => -a.
sup(r1, r2).
""")

arg2p.answer("a").in_statements     # ['a']
arg2p.answer("-a").out_statements   # [Struct(functor='-', args=('a',))], written -a
```

`Arg2p.of(theory, flags)` is a solver for a theory (`Flags.defaults()` if no flags are given), `Arg2p.builder()`
describes one in full (`theory`, `flags`, `output`, then `build()`). `add(text)` adds clauses to the theory,
`load(text)` replaces it.

`Flags.defaults()`, `Flags.none()` (only the flags the theory states) and `Flags.parse(text)` are the flags of a
solver; each flag has a method, taking a value of its enumeration (`Profile`, `Semantics`, `StatementLabelling`,
`OrderingPrinciple`, `OrderingComparator`, `GraphExtension`, `GraphLanguage`, `GraphBuilder`, `ConflictModel`) or a
boolean:

```python
from arg2p import GraphExtension, Profile

flags = (
    Flags.defaults()
    .graph_build_mode(Profile.ABA)
    .argument_labelling_mode(Semantics.PREFERRED)
    .graph_extensions(GraphExtension.STANDARD_PREF)   # no argument: none
    .query_mode(False)
)
```

A theory its profile refuses (a non-flat ABA theory, say), or a query that cannot be parsed, raises `Arg2pError`,
with the reason. When a text does not parse, its `syntax` tells where: a `SyntaxProblem` with the `line` and `column`
(from 1), the `token` found there and the `description`. `Arg2p.check_theory(text)` and `Arg2p.check_query(text)` tell
the same without solving anything (None for a text that parses).

```python
Arg2p.check_theory("f1 :=> a.\nr1 : a => b(.")    # Unexpected token '.'; expected term at 2:13
```

---

### Graphs

`graphs()` gives the graph of each labelling (several for a semantics such as `preferred`), as plain data:

```python
(graph,) = Arg2p.of("f1 :=> d.\nf2 :=> -d.\n").graphs()
graph.conclusions("und")                     # ['-d', 'd']
[it.descriptor for it in graph.arguments]    # ['A0 : f2 : -d', 'A1 : f1 : d']
graph.attacks[0].attacker.identifier          # 'A1'
```

A graph has its `arguments`, `attacks`, `supports` and `labellings`, and `label(argument)`, `labelled(label)` and
`conclusions(label)`. `active_branch`, `graph()`, `graph_at(branch)` and `context_facts(branch, goal)` read the
stages of the last resolution.

---

### Prolog goals

`solve(goal)` gives the solutions of a goal one at a time (`has_next()` and `next()`, or iterated). Their values are
Python values: atoms as strings, numbers as numbers, lists as lists, compound terms as `Struct` and variables as
`Var`.

```python
[solution.value("X") for solution in arg2p.solve("member(X, [a, b])") if solution.is_yes]   # ['a', 'b']
```

---

### Causality

```python
causality = Arg2p.of("r1 :=> a.\nr2 : a => b.").causality
causality.ness("a", "b")      # the intervention, [Struct(functor='-', args=('a',))]: None if a is not a cause
causality.but_for("a", "b")   # True
```

---

### The parallel solver

A `ThreadedCluster` builds the graph with workers on the threads of the JVM, and with those of the clusters it is
federated with (on the JVM, Node.js or in browsers), over WebSockets:

```python
from arg2p import ThreadedCluster

with ThreadedCluster(threads=1) as evaluating, ThreadedCluster(threads=4) as lending:
    port = evaluating.listen(0, "127.0.0.1")
    lending.connect(f"ws://127.0.0.1:{port}")   # or discover() on both, on a local network
    evaluating.load(theory)
    evaluating.solve("a")                       # EvaluationResponse(in_args=[Response(claim='a', ...)], ...)
```

A `LocalCluster` runs its nodes in turn, on the calling thread.

The `solver` of a cluster solves any goal as `Arg2p` does, the graph built by the workers (see the
[parallel solver]({{% ref "/docs/modules/parallel-solver#as-a-solver-of-arg2p" %}})), and `stats` tells what the
federation does:

```python
solver = evaluating.solver
solution = solver.solve("buildLabelSets(In, Out, Und)").next()
solution.text("In")                          # as for Arg2p
solver.graph()                               # the Graph of the solution
solver.answer("a").in_statements             # as Arg2p.answer
evaluating.stats.evaluation.workers          # [WorkerStats(partition=0, site='site-...', arguments=..., attacks=...), ...]
evaluating.flags = Flags.defaults().query_mode(False)   # for the next evaluations, the peers kept
solver.stop()                                # from another thread: the goal being solved ends as a halt
```

---

### From the command line

```sh
arg2p theory.pl                    # the statements of each labelling, by label
arg2p theory.pl -q a --query=-a    # the labels of some statements
arg2p theory.pl -p aba -s preferred
```

`arg2p mcp` runs the [MCP server]({{% ref "/docs/run#ai-assistants-mcp" %}}) of arg2p instead, over the standard
input and output, until its client closes them: `uvx arg2p mcp` with [uv](https://docs.astral.sh/uv/), installing
nothing first. A theory in a file named `mcp` is `arg2p ./mcp`.
