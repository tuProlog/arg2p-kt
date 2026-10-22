# Arg2p engine

A fork of the classic Prolog solver of [2p-kt](https://github.com/tuProlog/2p-kt) (module `solve-prolog`, version
3.0.2), licensed under the [Apache License 2.0](https://www.apache.org/licenses/LICENSE-2.0). The package was renamed
from `it.unibo.tuprolog.solve.prolog` and `PrologSolverFactory` to `Arg2pPrologSolverFactory`; the rest of 2p-kt
(terms, unification, theories, libraries) is used as it is.

The solvers of arg2p are created through `Arg2pPrologSolverFactory`, a `SolverFactory` like any other: the fork lets
the engine be tuned for the argumentation workloads (deep resolution over large terms, many sub-solves) while
keeping the semantics of the original solver.

## Changes from the original

None yet: the first version is the original code, with the package renamed.
