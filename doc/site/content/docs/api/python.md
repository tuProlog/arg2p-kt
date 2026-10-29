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
                                                # (connect(uri, code) to one requiring a code: requires_code, code)
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

Discovering with a code required (`discover(require_code=True)`), the clusters heard are `nearby` alone, linked to
once paired with their code (`pair(site, code)`; see the
[parallel solver]({{% ref "/docs/modules/parallel-solver#with-a-code" %}})).

---

### From the command line

```sh
arg2p theory.pl                    # the statements of each labelling, by label
arg2p theory.pl -q a --query=-a    # the labels of some statements
arg2p theory.pl -p aba -s preferred
```

`arg2p mcp` runs the [MCP server]({{% ref "/docs/run#ai-assistants-mcp" %}}) of arg2p instead, over the standard
input and output, until its client closes them: `uvx arg2p mcp` with [uv](https://docs.astral.sh/uv/), installing
nothing first. A theory in a file named `mcp` is `arg2p ./mcp`. On Linux and macOS the server takes the place of the
`arg2p` process, signals included; on Windows it runs as its child, and ends with it.

### The recorded page

`arg2p page` writes an HTML page showing a query solved on a theory, with the solver of the
[playground](https://tuprolog.github.io/arg2p-kt-web/): its solutions, the argument graph of each, the flags in
effect and the theory, and a link opening them in the playground. The page solves nothing: it shows the run as it was
solved, which is why it opens where no solver runs beside it (the sandboxes of AI assistants: claude.ai shows the file
live in the conversation). Another query, an edited theory or other flags are solved again, in a new page or in the
playground.

```sh
arg2p page theory.pl                                     # writes arg2p-theory.html, and prints its path
arg2p page theory.pl -q "flies(X)" -f "argumentLabellingMode(complete).\nqueryMode." -o explore.html
arg2p page - --max-solutions 50 --timeout 120 < theory.pl
```

By default the query gives the statements labelled in, out and und, and the flags (Prolog facts, `\n` a new line) are
those of arg2p. The page keeps the first solutions (`--max-solutions`) found within a time limit (`--timeout`), and
tells when there were more or the time ran out; `arg2p page --help` tells the defaults, as `arg2p.recorded.defaults()`
gives them. A theory that does not parse is shown with its error.

From Python, `page` gives the HTML:

```python
from arg2p import Flags, Semantics, page

html = page(theory, "buildLabelSets(In, Out, Und)", Flags.defaults().argument_labelling_mode(Semantics.COMPLETE))
html = page(theory, flags="argumentLabellingMode(complete).", max_solutions=50, timeout=120)
```

Its components come from [jsDelivr](https://www.jsdelivr.com/package/npm/@tuprolog/arg2p-ui): the page needs the
network to show them.
