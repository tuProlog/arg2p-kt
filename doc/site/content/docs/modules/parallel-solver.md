---
title: Parallel Solver
weight: 29
---

## Parallel Solver

The parallel solver builds the argumentation graph of a theory with several workers at once, then labels it as the
standard solver does. A coordinator, the **catalog**, splits the theory among the workers; each worker builds the
arguments of its own rules, and finds the attacks on them. The answers are the same as the standard solver's, with
any semantics and graph extension; the construction of the graph is what runs in parallel.

It is meant for theories large enough that building the graph dominates the evaluation. On a small theory the
coordination costs more than it saves.

The module is available for the JVM and for JavaScript as `parallel-solver` (see [Running]({{% ref "/docs/run" %}})).

> [!NOTE]
> This module is more experimental than the rest of the framework: expect its API to change more often than the
> core engine. The [actor solver]({{% ref "/docs/modules/actor-solver" %}}) is a different design, with nodes that
> argue over their fragments of the theory and combine their labels.

---

## Runtimes

The catalog and the workers are the same on every runtime; only the way their messages travel changes.

| Class | Runtime | Workers |
| --- | --- | --- |
| `LocalCluster` | JVM, JavaScript | Taking turns on the calling thread: the reference behaviour, for tests and debugging. |
| `ThreadedCluster` | JVM | On a pool of threads, as many as the processors by default. |
| `JsWorkerCluster` | Node.js | On `worker_threads`, each loading this module. |

All of them take the number of partitions of the theory (`parts`, by default as many as the threads), and the JVM
ones the settings of the solver (`FlagsBuilder`), which the flags written in the theory complete, as for the standard
solver.

---

## On the JVM

```kotlin
import it.unibo.tuprolog.argumentation.core.runtime.settings.FlagsBuilder
import it.unibo.tuprolog.argumentation.parallel.cluster.ThreadedCluster

ThreadedCluster(threads = 8, settings = FlagsBuilder().argumentLabellingMode("complete")).use { cluster ->
    cluster.load(theory)
    val answer = cluster.solve("conclusion")
    println(answer.inArgs.map { it.claim })
}
```

`load` replaces the theory, `add` adds a clause, `reset` forgets the theory. The first `solve` after a change
builds and labels the graph; the following ones read the labels. `solve` may be called from several threads at
once.

---

## On Node.js

```javascript
const { JsWorkerCluster } = require('@tuprolog/arg2p-parallel-solver').it.unibo.tuprolog.argumentation.parallel.js

const cluster = new JsWorkerCluster(4)
cluster.load(theory)
const answer = await cluster.solve('conclusion')
console.log(answer.inArgs.map(r => r.claim))
cluster.close()
```

The worker threads keep Node.js running only while an evaluation is pending; `close` stops them. `JsLocalCluster`
offers the same methods on the calling thread, with `solve` returning the answer directly.

---

## How the graph is built

1. The catalog splits the theory into partitions of related rules (those whose conclusions and premises unify), the
   largest split further so that every worker has a share. Each worker receives its rules and tells the catalog the
   premises they use.
2. Each worker builds the arguments of its rules and sends every new argument directly to the workers whose rules
   use its conclusion, which build on it in turn, and publishes it to the catalog.
3. Once every argument is built, each worker asks the others for the arguments that could attack its own, and checks
   each such pair once. Only the direct attacks are sent to the catalog: the graph store extends them to the
   arguments built on the attacked ones.
4. The catalog labels the graph with the settings of the solver, as the standard solver does.

With `metaConflicts`, where conflicts are themselves arguments, the catalog finds the attacks itself.
