# Module parallel

The parallel solver of arg2p, in three parts.

**The store** (`store`). Several solvers build the graph of a theory together, each with its part of the rules, through
a store they share (`SharedStore`). None of them knows of the others: each runs the pipeline of the standard solver, the
modules unchanged, and the sharing lives in the graph store (`GraphSharing` in the core, implemented by `SharedGraph`). What
a solver saves is published as a segment of graph facts, and merged by the others into their stores. While the graph is
built a solver enumerates what it owns and looks up what every solver built, so each pair is checked once. What a solver
sees of the store is a `StoreConnection`: by reference in the same process (`DirectConnection`), as text otherwise
(`MessageConnection`, over the `StoreProtocol`).

**The consumers** (`consumer`). A consumer runs the pipeline on its part of the rules (`TheoryPartitioner`, `ConsumerStart`).
The solver a goal was asked of (`ClientSolver`) is a consumer too, the first of its graph: it also answers the clients, and
labels the graph, with any semantics and graph extension.

**The hosts** (`host`). A `ConsumerHost` is a place where consumers can run, reached through `HostFrame`s, as text: the
threads of the solver (`ConsumerThreads`) or anything else. A `ParallelNode`, the part of a solver that never waits, keeps the
stores, and places the consumers of each graph on its hosts in proportion to what each offers. How a thread waits is all that
changes from a platform to another (`Inbox`: a queue on the JVM, a SharedArrayBuffer in JavaScript).

The solvers are `ThreadedSolver` on the JVM, `ThreadedSolver` on Node.js and `WebWorkerSolver` in browsers; each is also a
solver of arg2p, with the interface of `Arg2p` (`GoalSolver`, `AsyncGoalSolver` in JavaScript). All of this happens in one process; to build
its graphs with other instances, see the module federation.
