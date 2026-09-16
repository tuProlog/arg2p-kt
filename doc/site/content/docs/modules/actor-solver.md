---
title: Distributed Solver
weight: 27
---

## Distributed Solver

The actor solver evaluates a theory across a cluster of actors instead of a single engine. The knowledge base
is split among the nodes, each node argues over its own fragment, and the partial results are combined into
the final labelling. It is meant for theories large enough that a single solver becomes the bottleneck.

The module is **JVM only** — it is not published to npm — and is available as the Maven module
`actor-solver`. The cooperative evaluation it implements is described in Pisano, Calegari and Omicini,
_Multi-agent cooperative argumentation in Arg2P_; see [References]({{% ref "/docs/references" %}}).

> [!NOTE]
> This module is more experimental than the rest of the framework. The API described here is the one exercised
> by its test suite; expect it to change more often than the core engine.

---

## Getting a distributed solver

The entry point is the `parallel` extension on `Arg2pSolver`, which yields libraries to load into a 2P-Kt
solver:

```kotlin
import it.unibo.tuprolog.argumentation.actor.parallel
import it.unibo.tuprolog.argumentation.core.Arg2pSolver
import it.unibo.tuprolog.solve.classic.ClassicSolverFactory
import it.unibo.tuprolog.solve.flags.FlagStore
import it.unibo.tuprolog.solve.flags.TrackVariables
import it.unibo.tuprolog.solve.flags.Unknown

val solver = ClassicSolverFactory.mutableSolverWithDefaultBuiltins(
    otherLibraries = Arg2pSolver.parallel().to2pLibraries(),
    flags = FlagStore.DEFAULT
        .set(Unknown, Unknown.FAIL)
        .set(TrackVariables, TrackVariables.ON),
)
```

Both solver flags are required, exactly as when [assembling a solver by hand]({{% ref "/docs/kotlin-api" %}}).

---

## Predicates

The module is registered under the identifier `parallel` and provides:

| Predicate | Meaning |
| --- | --- |
| `join(Port)` | Starts or joins a cluster on the local machine at the given port. |
| `join(Port, Seed)` | Joins an existing cluster, where `Seed` is the `host:port` of a seed node. |
| `load` | Sends every clause of the static knowledge base to the cluster. |
| `solve(Goal, In, Out, Und)` | Evaluates the goal across the cluster, binding the three lists of labelled claims. |
| `reset` | Clears the distributed knowledge base. |
| `leave` | Clears the knowledge base and leaves the cluster. |

---

## A complete session

The order matters: join the cluster, load the theory, query it, then leave.

```kotlin
solver.loadStaticKb(Theory.parse(rules, solver.operators))

solver.solve(Struct.parse("join(2551), load")).first()

val answer = solver.solve(Struct.parse("solve(conclusion, In, Out, Und)")).first()
println(answer.substitution)

solver.solve(Struct.parse("leave")).first()
```

> [!WARNING]
> Always call `leave` when you are done. The cluster runs non-daemon threads, and a program that does not leave
> the cluster may fail to terminate.

---

## Splitting the knowledge base

Nodes receive fragments of the theory according to a splitting principle, and each node evaluates its fragment
with its own engine. Because the split is by support relations, conclusions whose arguments span several nodes
are resolved by exchanging partial results rather than by rebuilding the whole graph anywhere.
