# Arg2p engine

A fork of the classic Prolog solver of [2p-kt](https://github.com/tuProlog/2p-kt) (module `solve-prolog`, version
3.0.2), licensed under the [Apache License 2.0](https://www.apache.org/licenses/LICENSE-2.0). The package was renamed
from `it.unibo.tuprolog.solve.prolog` and `PrologSolverFactory` to `Arg2pPrologSolverFactory`; the rest of 2p-kt
(terms, unification, theories, libraries) is used as it is.

The solvers of arg2p are created through `Arg2pPrologSolverFactory`, a `SolverFactory` like any other: the fork lets
the engine be tuned for the argumentation workloads (deep resolution over large terms, many sub-solves) while
keeping the semantics of the original solver.

## Changes from the original

Each one keeps the semantics of the original solver; the timings are those of `EngineBenchmark` (CPU time).

- Variable tracking: the variables still of interest are collected in a hash set when a substitution is trimmed
  (membership was checked by scanning a sequence of the variables of all the frames above, for every binding); the
  goals selected in a frame are chained rather than copied into a set of their variables at each step; and the
  substitution is trimmed once it has doubled since it was last trimmed, rather than whenever a frame is left.
- Clause selection: the clauses for a goal are selected once (the procedure was first checked to have a matching
  clause, matching them all twice).
- Inlined calls (`InlinedCall`): a primitive can have the engine solve a goal in place of it, in a frame of its own
  with other libraries, rather than in a solver of its own. The module calls (`::`, `:::`) are inlined: no solver is
  set up for each call, the bindings get back as for any goal, and the call is subject to the time limit of the
  resolution. Backtracking into a choice point within an inlined call goes on with the libraries of the call.
- Clause selection (`ClauseIndex`): the clauses of the immutable theories (libraries, static theory) are grouped by
  the signature of their heads once, and those matching a goal are collected in a list, each renamed only when tried
  (lookups merged the lazy sequences of the RETE indexes of the theories at every step); a mutable theory gives the
  clauses it has when the goal is selected.
- Rule execution: the body of a rule is built with the unifier of its head and the goal, and only the bindings of
  the variables of the goal are added to the substitution of the resolution (those of the fresh copy of the rule
  only occur in its head and body).
- call/1: solved as its clause would (`call(G) :- ensure_executable(G), G`), in a frame of its own opaque to cut,
  without selecting the clause and running `ensure_executable/1` as a goal of its own.
- Steps: the instant of each state is only read with a time limit, the lazy values of contexts and states are not
  synchronized (a resolution is confined to a thread), and the path of a context to the root is built when needed.

The unification used by the engine is the one of `Arg2pUnificator` (in `libs.basic`): the default one, computed in a
single recursive pass rather than by rewriting a queue of equations (`Arg2pUnificatorTest` compares the two).

Together, against the original solver: graph building 3.5-4.7x faster, query mode 4.2x, resolution 5.2x, module
calls 1.3-3.8x, labellings 2.2-2.4x.
