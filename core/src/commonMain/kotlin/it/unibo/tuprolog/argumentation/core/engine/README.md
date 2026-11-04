# Arg2p engine

The resolution engine of arg2p: a machine laid out as SWI-Prolog lays its own, written in Kotlin for every target,
behind the interfaces of [2p-kt](https://github.com/tuProlog/2p-kt) (terms, unification, theories, libraries,
primitives, `Solver`), which are used as they are. The solvers are created through `Arg2pPrologSolverFactory`, a
`SolverFactory` like any other.

## The machine (`vm`)

- `Words`: a term is a 32-bit word (three tag bits: a reference, an atom, a small integer, a compound term, a list
  cell, a float or a large integer by table, a functor) in a heap of ints; a variable is a cell referring to itself
  while unbound. 32 bits keep the machine the same in JavaScript.
- `Program`: the symbol tables (equal constants are equal words), the code array the clauses compile into, the
  predicates of the theory of the user and, for each set of libraries a resolution runs with, a `Scope` of the
  predicates of their clauses and the primitives they provide. A predicate of eight clauses or more indexes its
  clauses by the key of their first argument.
- `Compiler`: a clause into the instructions of `Op` (those of the WAM): the head unified against the argument
  registers by `get` and `unify` instructions that know its shape, the arguments of each body goal put in the
  registers, the last goal executed in place of the clause. The variables of one chunk live in registers, those
  spanning several body goals in a frame of the local stack; a variable occurring once is void. Conjunction,
  disjunction, if-then-else and negation as failure compile inline (a cut inside a negation or a condition is local
  to it); the same constructs called as goals at run time go through hand-assembled blocks. A control construct the
  theory of the user gives clauses to (the strict rules of a theory are clauses of `->/2`) is compiled as a goal.
- `Machine`: the heap, a trail of the cells bound since the latest choice point, the local stack of frames, the
  choice points as records of ints, the registers. Backtracking resets the tops and undoes the trail. The heap is
  garbage collected (a sliding mark-compact: the cells reachable from the registers of the call at hand, the live
  frames, the choice points, the trail and the words pinned around a nested run are kept in their order) when a call
  finds it above a threshold. Exceptions are balls unwound to the nearest `catch/3`, a choice point of its own;
  `findall/3` runs its goal as a nested run behind a barrier choice point and copies its answers through a bag.
  Integer arithmetic is evaluated natively, anything else by the library.
- The facade: a goal no builtin nor clause answers is given to the primitive of the libraries as tuProlog terms
  (the context is `PrologExecutionContext` over the `ExecutionEnvironment` of the resolution); its answer is unified
  back, its further answers left as a choice point holding the responses (an error computing one ahead is raised
  when the resolution backtracks into it), its side effects applied to the environment and mirrored into the
  program; any exception of its code is a `system_error` `catch/3` can handle.
- `NativePrimitive`: a primitive the machine runs on its words (through `NativeCall`: the arguments read,
  unified and built in place, a goal entered against other libraries, alternatives left for backtracking), skipping
  the facade; the module calls `::` and `:::` are native. A `Primitive` still, for the other solvers.
- `Record`: a term off the heap as the words of a program (relative addresses, its variables fresh each time it is
  put back), what the context and the cache of arg2p keep: a native takes one from the heap and puts it back, enters
  a goal with records as the clauses of its predicate, or runs a nested run (`findall`, a count of solutions) for
  itself. The stores are bound to the solver's program when it is built, so their records are in the words of the
  machine: a lookup unifies the head of a record with the goal where it is on the heap, copying only what a variable
  of the heap is bound to, and leaves no choice point when no other record can match (see `Machine.storeGoal`). A
  predicate of a store is indexed by the top symbol of each argument position, a hash of the ground arguments when a
  bucket is large, and a hash of the whole head for goals with all their arguments bound, each index built when first
  needed and kept across assertions and removals (see `ClauseStore`). The other solvers read the records as terms.
- `SharedPrograms`: the clauses of the libraries are compiled once per process, into a base program per set of
  libraries; a solver's program starts as a copy of the base (its symbols and its code) and compiles the theory of
  the user alone. A base program grows as modules are first called; a solver uses the scopes compiled before its
  copy, and compiles the others itself.
- `VmSolver` and `MutableVmSolver`: the tuProlog `Solver` over a program compiled from the static and the dynamic
  knowledge base (again when either changes through the solver); the solvers created for the sub-goals of a
  resolution share the program. The theory of the user is a module of its own: a predicate it defines is its, whatever
  the libraries define under the same name, and the modules see it only through `user::`.

A cell holding an unbound variable remembers what it is called when the term is read: a variable the code made is
read fresh under its name, a variable of a term given to the machine is read as that very object (a stored term read
twice gives the same variables). The bindings of a solution are reported in the order they were made.

`ClauseObservers` are told of every clause the machine enters (an instrument for coverage reports).
