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
- Delegated calls (`DelegatedCall`): a primitive can have the engine solve its goal itself, as a sub-goal of the
  same resolution, in a frame of its own opaque to cut (`DelegationStates`). An `Inlining` solves a goal in place
  with other libraries: the module calls (`::`, `:::`) are inlined, so no solver is set up for each call, the bindings
  get back as for any goal, and the call is subject to the time limit of the resolution; backtracking into a choice
  point within an inlined call goes on with the libraries of the call. A `Sourcing` resolves a goal with the clauses
  of a `ClauseSource`: the lookups of the context (`context_check/1,2`) are resolved with the clauses of a branch
  (`BranchClauses`, in `libs.basic`), as the context store answers them.
- Clause selection (`ClauseSource`): the clauses of the program are those of the libraries, the static and the dynamic
  theory (`ProgramClauses`). Those of the immutable theories are indexed once by the signature of their heads, then
  by the principal symbol of their first argument (`IndexedTheoryClauses`, `FirstArgumentIndex`), and those matching
  a goal collected in a list (lookups merged the lazy sequences of the RETE indexes of the theories at every step:
  looking up one of 2000 facts by its first argument is 10x faster); a mutable theory gives the clauses it has when
  the goal is selected.
  The indexes are kept by each solver (`TheoryIndexes`, in its execution contexts) and shared with the solvers its
  resolutions create, which run in its thread: solvers in different threads have indexes of their own.
- Rule execution: the body of a rule is built with the unifier of its head and the goal, and only the bindings of
  the variables of the goal are added to the substitution of the resolution (those of the fresh copy of the rule
  only occur in its head and body). A variable of the rule meeting one of the goal is bound to it, rather than the
  other way round, so a goal passing its variables to a rule adds no binding: enumerating a list with member/2 was
  cubic in its length (2000 elements: 112 s -> 0.44 s).
- Clause resolution (`ClauseResolver`): with the unificator of arg2p, the head of a clause as stored is unified with
  the goal, and the body built from the result, the variables of the clause being renamed on the way
  (`StoredClauseResolver`); with any other unificator, the clause is copied as a whole, then its head unified, then
  its body prepared for execution and built with the unifier (`RenamingClauseResolver`). Graph building and query
  mode 1.22-1.24x faster; `ClauseResolverTest` compares the two on random clauses and goals. The body of a fact is
  not selected as a goal.
- Control constructs (`ControlConstruct`): predicates the engine solves natively, as their clauses in the libraries
  would, without selecting them and solving their goals one by one. The meta-calls (`MetaCall`) call/1 and \+/1 are
  solved in a frame of their own opaque to cut, after the check of ensure_executable/1: call/1 as
  `call(G) :- ensure_executable(G), G`, \+/1 as `\+ G :- ensure_executable(G), call(G), '$cut', fail` with
  `\+ G :- true` as the alternative of the frame.
- Solvers: the operators of the libraries are collected once when a solver is created (they were collected again for
  its first context), which halves the cost of the solvers created for sub-goals (findall/3, the primitives).
- Steps: the instant of each state is only read with a time limit, the lazy values of contexts and states are not
  synchronized (a resolution is confined to a thread), and the path of a context to the root is built when needed.

The unification used by the engine is the one of `Arg2pUnificator` (in `libs.basic`): the default one, computed in a
single recursive pass rather than by rewriting a queue of equations (`Arg2pUnificatorTest` compares the two).

Together, against the original solver (`EngineBenchmark`, CPU time): graph building 4.4-5.4x faster, query mode
4.7x, resolution 5.7x, labellings 3-3.2x, module calls 1.5-3.8x. On whole theories (`LabellingBenchmark`, wall
clock), the phases of graph building are 2.3-4.9x faster: chain/320 (6080 arguments) went from about 34 s to 8.6 s.

The other arg2p changes that go with the engine: the native primitives unify with `Arg2pUnificator`, reach the
context library directly (`Context.of`) rather than solving goals, and the scopes of `arg2pScope` share their solver.
