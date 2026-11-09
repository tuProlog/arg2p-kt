---
title: Parallel Solver
weight: 29
---

## Parallel Solver

The parallel solver builds the argumentation graph of a theory with several solvers at once, then labels it as the
standard solver does. The theory is split among them; each one, a **consumer** of the graph, runs the pipeline of the
standard solver on its part of the rules, and what it builds reaches the others through a graph store they share. The
answers are the same as the standard solver's, with any semantics and graph extension; the construction of the graph is
what runs in parallel.

It comes in three layers:

| Layer | What it is |
| --- | --- |
| **The store** | The graph every consumer builds on, unaware of the others (the graph store of the core, shared through `SharedGraph`) |
| **The consumers** | The solvers running the pipeline, each on its rules; the one a goal was asked of (`ClientSolver`) also labels the graph |
| **The hosts** | Where the consumers run, and how they reach the store: the threads of the parallel solver, a [federation](#federation), or any other medium (`ConsumerHost`) |

It is meant for theories large enough that building the graph dominates the evaluation. On a small theory the
coordination costs more than it saves.

The module is available for the JVM and for JavaScript as `parallel`, working within one process, and what links several
instances as `federation` (see
[Running]({{% ref "/docs/run" %}})).

> [!NOTE]
> This module is more experimental than the rest of the framework: expect its API to change more often than the
> core engine. The [actor solver]({{% ref "/docs/modules/actor-solver" %}}) is a different design, with nodes that
> argue over their fragments of the theory and combine their labels.

---

## Runtimes

The solvers are the same on every runtime; only where they run, and how what they build travels, changes.

| Class | Runtime | Where it runs |
| --- | --- | --- |
| `ThreadedSolver` | JVM | On threads of the JVM, as many as the processors by default. |
| `WorkerSolver` | Node.js | On `worker_threads`, each loading this module. |
| `WebWorkerSolver` | Browsers | On Web Workers, each running a script that loads this module. Needs a page [isolated across origins](#isolation) to run more than one solver. |

All of them take the number of `threads` (the most consumers a graph has on that parallel solver), the number of partitions of the
theory (`parts`, by default as many as the consumers the parallel solver and its federation can run), and the flags of the solver
(`Flags`, as for the [API]({{% ref "/docs/api/kotlin" %}})), which the flags written in the theory complete, as for the
standard solver. The flags can be changed at any time (`flags`, a property of every parallel solver): the next evaluation uses
them, the graph built anew, as when the theory changes. They are in the
aggregate packages (`arg2p`, the npm package `@tuprolog/arg2p`, the Python package `arg2p`) as well.

---

## On the JVM

```kotlin
import it.unibo.tuprolog.argumentation.Flags
import it.unibo.tuprolog.argumentation.Semantics
import it.unibo.tuprolog.argumentation.parallel.ThreadedSolver

ThreadedSolver(threads = 8, flags = Flags.defaults().argumentLabellingMode(Semantics.COMPLETE)).use { parallel ->
    parallel.load(theory)
    val answer = parallel.solve("conclusion")
    println(answer.inArgs.map { it.claim })
}
```

`load` replaces the theory, `add` adds a clause, `reset` forgets the theory. The first `solve` after a change
builds and labels the graph; the following ones read it. `solve` may be called from several threads at
once. A statement that cannot be read is an error: `solve` raises an `Arg2pException` (`Arg2pError` in
Python, the promise is rejected in JavaScript), and the parallel solver answers the following ones as before.

`solve` gives the statements answering a statement, by label, as `solve/4` labels them (under the flag
`answerMode` as well), each with the arguments for it (`queryChain`); an error a consumer meets building (a Prolog check
of a rule that raises one) is the error of the evaluation, as the standard solver raises it; any other goal is for the
parallel solver as a solver of arg2p (`goals`, see [below](#as-a-solver-of-arg2p)).

---

## On Node.js

```javascript
import { WorkerSolver } from '@tuprolog/arg2p'

const parallel = new WorkerSolver(4)
parallel.load(theory)
const answer = await parallel.solve('conclusion')
console.log(answer.inArgs.map(r => r.claim))
await parallel.close()
```

Nothing of the parallel solver runs on the calling thread but this object, so a long evaluation does not block it. `threads`
consumers run on as many threads, the first being the client solver, and one thread more runs its node, which
never waits. The threads keep Node.js running only while an evaluation is pending; `close` stops them, rejecting the evaluations pending,
and its promise tells once they stopped. If one of the threads dies, the evaluations pending and the following ones are
rejected with the reason.

---

## As a solver of arg2p

Every parallel solver is also a solver of arg2p with the interface of `Arg2p` (see the [API]({{% ref "/docs/api/kotlin" %}})),
on its theory and its flags: `goals` solves any goal as the standard solver does (`arg2p::solve(In, Out, Und)`,
`solve/4`, the Prolog predicates of the theory, ...), one solution at a time, and gives the graph of each
solution and the answers of queries, in the types of the API (`Solutions`, `Solution`, `Answer`, `Graph`).

```kotlin
val solver = ThreadedSolver(threads = 8).apply { load(theory) }.solver
val solution = solver.solve("arg2p::solve(In, Out, Und)").next()
solution.text("In")                  // [[a], [c]]
solver.graph()                       // the graph of the solution: arguments, labels, attacks, supports
solver.answer("a").single().inStatements   // [a]
solver.graphs()                      // the graph of each labelling
```

| Member | Meaning |
| --- | --- |
| `solve(goal)` | The solutions of the goal, Prolog text, given one at a time (`Solutions`); a goal that cannot be read is an error |
| `graph()` | The graph of the last solution given: the one of the last stage of its resolution |
| `answer(query)` | The statements for the query labelled in, out and und (`Answer`), as `Arg2p.answer` |
| `graphs()` | The graph of each labelling of the theory, as `Arg2p.graphs` |
| `output` | Where what the solver prints goes, as the solutions are given (the standard output by default) |
| `stop()` | Stops the goal being solved (see below); nothing is done if none is |

The parallel solvers of JavaScript promise what their threads compute: their `goals` (`AsyncGoalSolver`) gives a promise of
the solutions (`AsyncSolutions`, whose `hasNext()`, `next()` and `toArray()` are promises as well, each solution computed
as it is asked for), of the answer and of the graphs; `graph()` is the graph of the solution last given.

```javascript
const solutions = await parallel.goals.solve('arg2p::solve(In, Out, Und)')
while (await solutions.hasNext()) {
  const solution = await solutions.next()
  console.log(solution.text('In'), parallel.goals.graph().labellings.map(it => it.label))
}
```

How the goals are solved:

- the client solver solves the goal as the standard solver does, with the pipeline of the standard solver; it is
  a consumer of the graph itself, with the first part of the rules, and the graph beginning starts the others;
- a goal reading a graph built already reads it again, no other consumer involved; so does a goal solved again from its
  start, when its next solution is asked for after another goal was solved. The solutions given already are left out.
  What the goal does besides (asserting facts, printing) is done again: what was printed is given once;
- the client solver has the theory but the rules of the other consumers: a goal reading the rules themselves (the
  modules of the parser, the context) finds its own part alone;
- with several labellings (`preferred`, `stable`, ...), the labellings are the same as those of the standard
  solver, in an order of their own (that of the arguments the consumers built);
- the stages of a resolution (`graphAt`, `activeBranch` of `Arg2p`) are not kept: `graph()` is the last one.

`stop()` stops the goal being solved (on the JVM, from another thread): the solution waited for is at once a halt,
whose error is `The evaluation was stopped`, as the other halts of a goal (in JavaScript, the promise of the next
solution is fulfilled with it, as for any halt); so do the statements being evaluated with `solve`, and the answers
raise it. The graph being built for them is given up: what the consumers still compute for it reaches no later
evaluation (a graph has a session of its own). On the JVM the resolution under way is interrupted, and the next goals
are solved as before. In JavaScript nothing can interrupt a thread: the next goals are solved once the resolution under
way ends. A Prolog check of a consumer still running ends on its own, its result dropped.

---

## Statistics

`evaluation`, on every parallel solver, tells the last evaluation it answered, a statement or a goal labelling or reading a graph
(`EvaluationStats`), null before the first; a goal using no graph (a Prolog predicate) leaves it as it is.

An `EvaluationStats` has the part of the graph it was answered on (`locality`: `directional` or `connected` in query
mode, `whole_graph` otherwise, `alone` for a graph the client solver builds alone, a theory not split or an
abstract framework), the `arguments` and the `attacks` of the graph labelled, the arguments the consumers built for it
(`added`: all of them for a graph built anew, none for an evaluation answered on the graph as it was), what each consumer
built (`workers`, one for each partition, the client solver the first: its `partition`, its `site`, its
`arguments` and the `attacks` it saved), and how long it took (`milliseconds`).

---

## Federation

Instances of the parallel solver, in the same process, on the same machine or on different ones, can be federated: each one
keeps its own theory, and runs consumers of the graphs of the others. A graph is then built by consumers on every instance
at once, the theory split among them in proportion to what each one offers. The engines can be mixed: a JVM instance, a
Node.js one and a browser can build a graph together.

A parallel solver knows nothing of this: a `Federation` stands on it, making the peers it links to hosts of its consumers. It
closes with the solver, or before it (`close`), the solver going on alone. With `lends` false it does not run the consumers of
the others.

One instance listens, the others connect to its URL. The consumers of a graph reach it through the instance that asked for
it, so two instances lending to the same graph need no link between them.

```kotlin
// on one machine
val hub = ThreadedSolver(threads = 8)
val port = Federation(hub).listen(9431)

// on another
val other = ThreadedSolver(threads = 16)
Federation(other).connect("ws://hub-host:9431")
other.load(theory)
other.solve("conclusion") // built by consumers on both
```

```javascript
const parallel = new WorkerSolver(4)
await new Federation(parallel).connect('ws://hub-host:9431')
```

| | JVM | Node.js | Browser |
| --- | --- | --- | --- |
| Connect to a URL | yes | yes | yes |
| Listen | yes, `ws` and `wss` | yes, `ws` | no |

A browser cannot listen, so two browsers cannot link to each other: each links to a JVM or Node.js instance. A graph is built
on the instance asking and on those linked to it, so a browser asking for a graph uses that instance, and not another
browser linked to it.

A parallel solver of JavaScript lends all its threads but the one of its solver; its own graphs still run while it lends,
each consumer on a thread of its own, more being made if they are all busy. If an instance leaves while a graph is being
built, the goal is solved again with the consumers left (up to three times); once built, the graph lives with the instance
that asked for it, and needs the others only when its theory changes. Only instances of the same version of arg2p federate.

`stats` tells what the federation is, cheap to read at any time (`FederationStats`): its `site`, and the `sites` it knows, itself
first (`SiteStats`: `id`, `name`, and `workers` it offers, null for no limit). The site that ran each consumer of an evaluation
is in the `evaluation` of the solver (`workers`). `onPeersChanged` is called whenever a site joins or leaves the peers: a page
can update its view without polling.

```javascript
federation.onPeersChanged = () => render(federation.stats.sites)
```

### With a code

An instance can require a code of those connecting to it (`requiresCode`): its `code`, four random digits (`newCode()` makes
another, the one before refused from then on). `connect(url, code)` presents it, and its promise is rejected (an
`Arg2pException` on the JVM) if the code is refused or no one answers in a few seconds. After three wrong codes from an address
its codes are refused for a while, a second after the third, twice as long after each one more (up to a quarter of an hour).

The code is never sent: each side proves it knows it. The instance that presented the right
code is paired with the other: it is given a token of its own, so that `connect(url)` to the same address links again with no
code while the two run, and the code is renewed for the next one.

```kotlin
val federation = Federation(ThreadedSolver(threads = 8))
federation.requiresCode = true
federation.listen(9431)
federation.code                         // 4831: told to whom links to this instance
```

```javascript
await new Federation(parallel).connect('ws://hub-host:9431', '4831')
```

The extension of VS Code federates its language servers this way, and lets phones join them: see
[VS Code]({{% ref "/docs/run#federation-and-parallel-solving" %}}).

### Pages on the port of an instance

A JVM instance answers the plain HTTP requests on the port it listens on as well, with the pages it is given
(`serve`): a page joining the federation, say, on the same address as the WebSockets.

```kotlin
federation.serve { path -> if (path == "/join") StaticContent(page, "text/html; charset=utf-8") else null }
federation.listen(9431)             // http://host:9431/join, and ws://host:9431 for the instances
```

A GET of a path is answered with its page (`StaticContent`: the bytes and their media type), isolated across origins,
any other request with 404; the requests opening a WebSocket are those of the instances, as before.

A browser on another host shares memory only over HTTPS (see [below](#isolation)). `listenSecurely` listens on a second
port over TLS, for the same pages and links: it takes an `SSLContext`, which `SelfSigned.context(addresses)` makes on
the spot for a local network, with a certificate the browser warns of once.

```kotlin
val secure = federation.listenSecurely(SelfSigned.context(listOf("192.168.1.17")), 9432)
// https://192.168.1.17:9432/join, and wss://192.168.1.17:9432 for the page to link to
```

### In a browser

`WebWorkerSolver` runs on Web Workers, each running a module of yours that calls `startWebWorker` (an ES module, loaded as a
module worker; with a bundler, the entry point of a worker bundle). Of the solver, the page keeps this object alone: `threads`
consumers on as many Web Workers, one Web Worker more for its node.

```javascript
// the page
const parallel = new WebWorkerSolver(4, '/arg2p-worker.js')
parallel.load(theory)
const answer = await parallel.solve('conclusion')

// arg2p-worker.js
import { startWebWorker } from '@tuprolog/arg2p'
startWebWorker()
```

To federate it, the script of its Web Workers is told how, as the site runs on the Web Worker of the node.

```javascript
// the page
const federation = new Federation(new WebWorkerSolver(4, '/arg2p-worker.js'))
await federation.connect('wss://hub-host:9431', '4831')

// arg2p-worker.js
import { startWebWorker, nodeService } from '@tuprolog/arg2p'
startWebWorker(nodeService)
```

#### Isolation

The consumers wait for what the others built on a `SharedArrayBuffer`, which a browser gives only to a page
**isolated across origins**: served with the headers below, in a secure context (HTTPS, or `localhost`). What the page
loads from other origins must allow it, as jsDelivr does; where the server cannot be changed, a service worker can add
the headers.

```
Cross-Origin-Opener-Policy: same-origin
Cross-Origin-Embedder-Policy: require-corp
```

On any other page (`crossOriginIsolated` is false) the solver still links to the other sites and answers, but it is one
solver alone on one Web Worker, and lends nothing: the graphs it asks for are built by it alone. A page on HTTPS links
over `wss://`, as browsers refuse `ws://` from it (see [above](#pages-on-the-port-of-an-instance)).

### Other media

A new medium is a link: something that carries text, in order, between two sites, whatever it is (WebRTC between browsers,
say). On the JVM it is a `Link`, given to the `site` of a federation as a `LinkListener`; in a page it is an `ExternalLink`, made
of how to send and how to close, told to the federation as it opens, as text comes and as it closes.

```javascript
const link = new ExternalLink(text => medium.send(text), () => medium.close(), /* outgoing */ true)
federation.linkOpened(link)
medium.onmessage = text => federation.linkReceived(link, text)
medium.onclose = () => federation.linkClosed(link)
```

The rest (the greetings, the codes, the consumers and what they ask of their store) goes over it as it does over a WebSocket.

> [!WARNING]
> A federated instance runs the theories its peers send it, and sends them its own. Listen only on networks you
> trust, or require a code.

---

## How the graph is built

1. The solver asked splits the theory into partitions of related rules (those whose conclusions and premises unify). A
   group larger than its share is split where it joins parts that support nothing of one another (as a rule using the
   conclusions of several chains), each part whole, so that they are built side by side; the largest partitions are
   split further, in halves, so that every consumer has a share. It keeps the first partition, and starts a consumer for
   each of the others, on a thread of its own or on a host lending one, with the rules of that partition and the flags
   of the solver.
2. Every consumer runs the pipeline of the standard solver on its rules, unaware of the others. What it saves in its graph
   store is published as a segment: the arguments, their infos and supports. When its argument builder asks what is new,
   the segments of the others are merged into its store, and it builds on the new conclusions its rules need. When
   nothing is new for any consumer, the arguments are all built.
3. Each consumer goes through its own arguments, and finds the attacks on them as the graph builder does, looking up the
   arguments of every consumer. Each direct attack is found by one consumer alone, and published as the arguments were.
4. The steps changing the graph (`modifyGraph`) run on every consumer, one at a time, each once every consumer has merged
   what the others did before it: the arguments a step adds are built by the consumer that owns the argument they come from,
   and an attack a consumer removes is removed by the others too.
5. The solver asked labels the graph, which it has whole, with the settings of the solver, as the standard solver does.
   The other consumers end.

### Who owns what

The extensions read the graph through the graph store, under one rule: **going through all the arguments (or supports,
or attacks) gives those of the consumer; asking for a specific one finds it, whichever consumer built it.** So nothing is
done twice, and any consumer can check anything. Outside a graph being built, and in the steps that do not divide the work,
every consumer goes through the whole graph.

| | Where it lives |
| --- | --- |
| Arguments, supports, attacks, attacks removed, statements explored | In the store of every consumer, once merged |
| Which of them a consumer owns | On that consumer |
| Identifiers of arguments and attacks | The same for the consumers of a process; each process numbers its own, segments between processes travel by content |
| Symbols, Prolog contexts, the cache of possible attackers | On each solver |
| The state of a graph (the order of the segments, what each consumer read, the end of each step) | On the node that asked for the graph (`SharedStore`), reached by its consumers through a `StoreConnection` |

Within a process the consumers read each other's segments in place; between processes the segments travel as Prolog text.
The state of a graph never waits: only the consumers wait, each on what it asked.

### Query mode

With `queryMode` (on in `Flags.defaults()`), the consumers build only the arguments the labels of the statements
evaluated depend on, as the semantics declares: exactly those the
[query mode]({{% ref "/docs/api/flags#querymode" %}}) of the standard solver builds, explored the same way, by the
same modules (`queryExploration`), each consumer on its own rules:

- every consumer is asked to explore a statement evaluated, those with no rule for it having nothing to do;
- a consumer explores it on its rules: the arguments for it, those for their premises, and the statements their labels
  may depend on (their possible attackers, and, for a semantics whose labels also depend on what an argument attacks
  or supports, the connected ones, those as well); then the statements the steps changing the graph relate to the
  arguments built (the conflicts with `metaConflicts`, the preferences with `defeasiblePref`, the burdens with `bp`),
  explored in turn;
- a statement explored goes through the graph store to the others, which explore it on their rules in turn, and what they
  build for it comes back with the segments, on which the consumer carries its exploration on. The premises of a rule are
  asked for all at once, rather than each once the one before it is supported as the standard solver does, so that the
  consumers build them side by side; should a premise have no argument, the arguments for the others are built nonetheless,
  which changes no label.

Once nothing is left to explore, the attacks are found and the graph is labelled as above. A later evaluation of
another statement builds its graph anew, as the standard solver does; one of the same statement is answered on the
graph built for it. A change of the theory starts again from scratch. The graph of an evaluation in query mode (see
`solver.graph()`) is thus that of its statement alone.

The whole graph is built with no query mode, for a semantics whose labels depend on all of it (`stable`), and for a
goal asking for it in query mode as well (`arg2p::solve(In, Out, Und)`, as the standard solver): an evaluation in query mode after
it builds the graph of its statements anew.
