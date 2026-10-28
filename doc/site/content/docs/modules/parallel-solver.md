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
| `JsWebWorkerCluster` | Browsers | On Web Workers, each running a script that loads this module. |

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
const cluster = new JsWorkerCluster(4)
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
await new JsWorkerCluster(4).discover()
```

### In a browser

`JsWebWorkerCluster` runs the workers on Web Workers, each loading a script of yours that loads the module and calls
`startWebWorker()` (with a bundler, the entry point of a worker bundle). A browser cannot listen nor send beacons: it
connects to a JVM or Node.js instance, and through it to the other browsers linked to that instance, over WebRTC.

```javascript
// the page
const cluster = new JsWebWorkerCluster(4, '/arg2p-worker.js')
await cluster.connect('ws://hub-host:9431')
cluster.load(theory)
const answer = await cluster.solve('conclusion')

// arg2p-worker.js
importScripts(/* the module and its dependencies */)
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

### Query mode

With `queryMode` (the default of `FlagsBuilder`), the graph is built only on the rules that the labels of the
statements evaluated depend on, as the semantics declares, like the
[query mode]({{% ref "/docs/api/flags#querymode" %}}) of the standard solver. The catalog finds those rules
before any argument is built:

- the rules concluding the statement, and those concluding their premises;
- the rules of the arguments that may attack them;
- for a semantics whose labels also depend on what an argument attacks or supports (the connected ones), those
  rules as well.

The workers then build the arguments of those rules alone. A later evaluation that needs more rules adds them to
the graph built; one that needs no more is answered from it. A change of the theory starts again from scratch.

The whole graph is built:

- with no query mode;
- for a semantics whose labels depend on all of it (`stable`);
- with `metaConflicts`;
- with a graph extension other than `standardPref`, `rebutRestriction`, `bp`, `defeasiblePref` and
  `defeasibleAllPref`;
- when the rules make too many statements to explore.
