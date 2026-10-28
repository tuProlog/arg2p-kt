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
| `WorkerCluster` | Node.js | On `worker_threads`, each loading this module. |
| `WebWorkerCluster` | Browsers | On Web Workers, each running a script that loads this module. |

All of them take the number of partitions of the theory (`parts`, by default as many as the threads), and the flags
of the solver (`Flags`, as for the [API]({{% ref "/docs/api/kotlin" %}})), which the flags written in the theory
complete, as for the standard solver. They are in the aggregate packages (`arg2p`, the npm package `@tuprolog/arg2p`,
the Python package `arg2p`) as well.

---

## On the JVM

```kotlin
import it.unibo.tuprolog.argumentation.Flags
import it.unibo.tuprolog.argumentation.Semantics
import it.unibo.tuprolog.argumentation.parallel.cluster.ThreadedCluster

ThreadedCluster(threads = 8, flags = Flags.defaults().argumentLabellingMode(Semantics.COMPLETE)).use { cluster ->
    cluster.load(theory)
    val answer = cluster.solve("conclusion")
    println(answer.inArgs.map { it.claim })
}
```

`load` replaces the theory, `add` adds a clause, `reset` forgets the theory. The first `solve` after a change
builds and labels the graph; the following ones read the labels. `solve` may be called from several threads at
once. A statement that cannot be read is an error: `solve` raises an `Arg2pException` (`Arg2pError` in
Python, the promise is rejected in JavaScript), and the cluster answers the following ones as before.

`solve` gives the arguments for a statement; any other goal is for the cluster as a solver of arg2p (`solver`, see
[below](#as-a-solver-of-arg2p)).

---

## On Node.js

```javascript
import { WorkerCluster } from '@tuprolog/arg2p'

const cluster = new WorkerCluster(4)
cluster.load(theory)
const answer = await cluster.solve('conclusion')
console.log(answer.inArgs.map(r => r.claim))
cluster.close()
```

The worker threads keep Node.js running only while an evaluation is pending; `close` stops them. `LocalCluster`
offers the same methods on the calling thread, with `solve` returning the answer directly.

---

## As a solver of arg2p

Every cluster is also a solver of arg2p with the interface of `Arg2p` (see the [API]({{% ref "/docs/api/kotlin" %}})),
on its theory and its flags: `solver` solves any goal as the standard solver does (`buildLabelSets(In, Out, Und)`,
`answerQuery/4`, the Prolog predicates of the theory, ...), one solution at a time, and gives the graph of each
solution and the answers of queries, in the types of the API (`Solutions`, `Solution`, `Answer`, `Graph`). The graph a
goal needs is built by the workers, the rest is the standard solver of the catalog.

```kotlin
val solver = ThreadedCluster(threads = 8).apply { load(theory) }.solver
val solution = solver.solve("buildLabelSets(In, Out, Und)").next()
solution.text("In")                  // [[a], [c]]
solver.graph()                       // the graph of the solution: arguments, labels, attacks, supports
solver.answer("a").inStatements      // [a]
solver.graphs()                      // the graph of each labelling
```

| Member | Meaning |
| --- | --- |
| `solve(goal)` | The solutions of the goal, Prolog text, given one at a time (`Solutions`); a goal that cannot be read is an error |
| `graph()` | The graph of the last solution given: the one of the last stage of its resolution |
| `answer(query)` | The statements for the query labelled in, out and und (`Answer`), as `Arg2p.answer` |
| `graphs()` | The graph of each labelling of the theory, as `Arg2p.graphs` |
| `output` | Where what the solver prints goes, as the solutions are given (the standard output by default) |

The clusters of JavaScript running their workers on threads (`WorkerCluster`, `WebWorkerCluster`) promise what the
workers compute: their `solver` (`AsyncClusterSolver`) gives a promise of the solutions (`AsyncSolutions`, whose
`next()` and `toArray()` are promises as well), of the answer and of the graphs; `graph()` is the graph of the
solution last given. A `LocalCluster` answers at once, in JavaScript as well.

```javascript
const solutions = await cluster.solver.solve('buildLabelSets(In, Out, Und)')
while (solutions.hasNext()) {
  const solution = await solutions.next()
  console.log(solution.text('In'), cluster.solver.graph().labellings.map(it => it.label))
}
```

How the goals are solved:

- the catalog solves the goal on its standard solver, whose graph builder (`ruleGraph`) and query mode
  (`queryExploration`) take the arguments and the attacks the workers built: the graph store extends the attacks, the
  solver derives the other arguments, applies the graph extensions and labels the graph, every semantics as the
  standard solver does;
- a goal needing a graph the workers did not build pauses: the workers build it (the whole graph for
  `buildLabelSets`, the part the statements explored depend on for `answerQuery` in query mode, explored further by
  the later goals), then the goal is solved again from its start. So is a goal whose next solution is asked for after
  the catalog solved another one; the solutions given already are left out. What the goal does besides (asserting
  facts, printing) is done again: what was printed is given once;
- the catalog has the theory but its rules, which the workers have: a goal reading the rules themselves (the modules
  of the parser, the context) does not find them;
- with several labellings (`preferred`, `stable`, ...), the labellings are the same as those of the standard
  solver, in an order of their own (that of the arguments the workers built);
- the stages of a resolution (`graphAt`, `activeBranch` of `Arg2p`) are not kept: `graph()` is the last one.

---

## Statistics

`stats` tells what a cluster and its federation do, cheap to read at any time (`FederationStats`):

| Field | Meaning |
| --- | --- |
| `site` | The identifier of the site of the cluster |
| `sites` | The sites it knows, itself first (`SiteStats`): `id`, `workers` it offers (null for no limit, or not known), `delegate` (one of its threads or Web Workers), `via` (the site it is reached through, null if linked directly) |
| `evaluation` | The last evaluation it answered, a statement or a goal using the graph (`EvaluationStats`), null before the first |

An `EvaluationStats` has the part of the graph it was answered on (`locality`: `directional` or `connected` in query
mode, `whole_graph` otherwise), the `arguments` and the `attacks` of the graph labelled, the arguments the workers
built for it (`added`: all of them for a graph built anew, those explored further in query mode, none for an
evaluation answered on the graph as it was), how many each worker built (`workers`: its `partition`, its `site`, its
`arguments` and the direct `attacks` it found), and how long it took (`milliseconds`).

`onPeersChanged`, on `ThreadedCluster`, `WorkerCluster` and `WebWorkerCluster`, is called whenever a site joins or
leaves the peers of the cluster: a page can update its view of the federation without polling.

```javascript
cluster.onPeersChanged = () => render(cluster.stats.sites)
```

---

## Federation

Instances of the parallel solver, on the same machine or on different ones, can be federated: each one keeps its
own theory and its own workers, and lends its workers to the others. A graph is then built by the workers of every
instance at once, the theory split among them in proportion to the workers each one runs. The engines can be mixed:
a JVM cluster, a Node.js one and a browser can build a graph together.

Instances federate over WebSockets: one listens, the others connect to it. An instance lends its workers to the
instances it is linked to, and borrows theirs; two instances building the same graph with no link between them
reach each other through the one whose graph it is.

```kotlin
// on one machine
val hub = ThreadedCluster(threads = 8)
val port = hub.listen(9431)

// on another
val other = ThreadedCluster(threads = 16)
other.connect("ws://hub-host:9431")
other.load(theory)
other.solve("conclusion") // built by the 24 workers of both
```

```javascript
const cluster = new WorkerCluster(4)
await cluster.connect('ws://hub-host:9431')
```

On a local network, instances can find each other instead: each one calling `discover()` listens on an ephemeral
port and announces itself every two seconds over UDP multicast (group `239.255.42.99`, port `9430`, not routed beyond
the network); the instances of the same version hearing it connect to it, and connect again if it comes back after
leaving.

```kotlin
ThreadedCluster(threads = 8).discover()
```

```javascript
await new WorkerCluster(4).discover()
```

### In a browser

`WebWorkerCluster` runs the workers on Web Workers, each running a module of yours that calls `startWebWorker()` (an
ES module, loaded as a module worker; with a bundler, the entry point of a worker bundle). A browser cannot listen nor
send beacons: it connects to a JVM or Node.js instance, and through it to the other browsers linked to that instance,
over WebRTC.

```javascript
// the page
const cluster = new WebWorkerCluster(4, '/arg2p-worker.js')
await cluster.connect('ws://hub-host:9431')
cluster.load(theory)
const answer = await cluster.solve('conclusion')

// arg2p-worker.js
import { startWebWorker } from '@tuprolog/arg2p'
startWebWorker()
```

The instance a browser connects to introduces it to the other browsers connected to it: the two open a WebRTC data
channel between them, and lend each other their Web Workers. On a local network they need nothing else; across
networks, give the cluster the STUN or TURN servers to use (`iceServers`, as `RTCPeerConnection` takes them).

Two browsers can also be paired by hand, with no server at all: one invites, the other accepts the invitation, and
the first completes the pairing with the reply. The codes (some 600 characters) can travel any way, pasted in a chat
or shown as a QR code; an invitation holds as long as the page that made it stays open.

```javascript
// first browser
const invitation = await cluster.invite()        // to send to the second
await cluster.complete(reply)                    // the reply of the second: linked

// second browser
const reply = await cluster.accept(invitation)   // to send back to the first
```

A Node.js cluster lends the workers of its threads as well: each thread is a site working for its cluster alone,
reached through it. Only instances of the same version of arg2p federate. If an instance leaves while a graph is
being built, the graph is built again with the workers left; once built, the graph lives with the instance that
asked for it, and needs the others only when its theory changes.

> [!WARNING]
> A federated instance runs the theories its peers send it, and sends them its own. Listen only on networks you
> trust.

---

## How the graph is built

1. The catalog splits the theory into partitions of related rules (those whose conclusions and premises unify). A
   group larger than its share is split where it joins parts that support nothing of one another (as a rule using the
   conclusions of several chains), each part whole, so that they are built side by side; the largest partitions are
   split further, in halves, so that every worker has a share. Each worker receives its rules and the flags of the
   solver, and tells the catalog the premises and the conclusions of its rules.
2. Each worker builds the arguments of its rules and sends every new argument directly to the workers that need its
   conclusion, which build on it in turn, and publishes it to the catalog.
3. Once every argument is built, each worker asks the others for the arguments that could attack its own, and checks
   each such pair once. Only the direct attacks are sent to the catalog: the graph store extends them to the
   arguments built on the attacked ones.
4. The catalog labels the graph with the settings of the solver, as the standard solver does: its graph builder and
   query mode save the arguments and the attacks the workers built (see [above](#as-a-solver-of-arg2p)).

With `metaConflicts`, where conflicts are themselves arguments, the catalog finds the attacks itself.

### Query mode

With `queryMode` (on in `Flags.defaults()`), the workers build only the arguments the labels of the statements
evaluated depend on, as the semantics declares: exactly those the
[query mode]({{% ref "/docs/api/flags#querymode" %}}) of the standard solver builds, explored the same way, by the
same modules (`queryExploration`), each worker on its own rules:

- the catalog asks the workers whose rules conclude a statement evaluated to explore it;
- a worker explores it on its rules: the arguments for it, those for their premises, and the statements their labels
  may depend on (their possible attackers, the conflicts that relate them with `metaConflicts`, what each graph
  extension declares, and, for a semantics whose labels also depend on what an argument attacks or supports, the
  connected ones, those as well);
- a statement explored that the rules of other workers conclude is explored by them in turn: they send back the
  arguments they build for it, on which the worker carries its exploration on (exploring again only what they may
  support). The premises of a rule are asked for all at once, rather than each once the one before it is supported
  as the standard solver does, so that the workers build them side by side; should a premise have no argument, the
  arguments for the others are built nonetheless, which changes no label.

Once nothing is left to explore, the attacks are found and the graph is labelled as above. A later evaluation that
needs more of the graph explores it further, then the attacks are found and the graph is labelled again; one that
needs no more is answered from it. A change of the theory starts again from scratch. The graph of an evaluation in
query mode (see `solver.graph()`) is thus the part built so far: that of its statements, and of those evaluated
before.

The whole graph is built with no query mode, for a semantics whose labels depend on all of it (`stable`), and for a
goal asking for it in query mode as well (`buildLabelSets`, as the standard solver): an evaluation in query mode after
it builds the graph of its statements anew.
