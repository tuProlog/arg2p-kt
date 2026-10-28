---
title: Kotlin
weight: 30
aliases:
  - /docs/kotlin-api/
---

## Kotlin API

There are two ways to use Arg2P from Kotlin or Java:

1. **The API of arg2p** — `Arg2p`, a solver for a theory: its answers, graphs and solutions as Kotlin objects, its
   flags as enumerations, and the causality tests. Start here: it is the same API in
   [JavaScript]({{% ref "/docs/api/javascript" %}}) and, in snake case, in [Python]({{% ref "/docs/api/python" %}}),
   and nothing of 2P-Kt is needed.
2. **The 2P-Kt integration** — load Arg2P as a library into your own `Solver` and query it with Prolog goals.
   Use this when you need the full Prolog interface, custom libraries, or an existing 2P-Kt setup.

The API is in the aggregate artifact `arg2p` (`arg2p-jvm` on the JVM), with every module of Arg2P (the core, the
causality and the parallel solver), package `it.unibo.tuprolog.argumentation`.

---

## A solver

`Arg2p.of(theory, flags)` is a solver for a theory, Prolog text, with its flags (`Flags.defaults()` if left out):

```kotlin
import it.unibo.tuprolog.argumentation.Arg2p

fun main() {
    val arg2p = Arg2p.of(
        """
        r1 : [] => a.
        r2 : [] => -a.
        sup(r1, r2).
        """.trimIndent(),
    )
    println(arg2p.answer("a").inStatements)   // [a]
}
```

`Arg2p.builder()` describes one in full: its `theory`, its `flags`, and the `output` receiving what it prints (the
standard output otherwise). `add(text)` adds clauses to the theory, `load(text)` replaces it, and `theory` is the
theory as Prolog text. `Arg2p.version()` is the version of Arg2P.

### Flags

`Flags.defaults()` are the [flags]({{% ref "/docs/api/flags" %}}) of Arg2P by default, `Flags.none()` only those
the theory states, and `Flags.parse(text)` those stated by Prolog facts. Each flag has a method, taking a value of
its enumeration, or a boolean for a switch:

```kotlin
val flags =
    Flags.defaults()
        .graphBuildMode(Profile.ASPIC_PLUS)
        .argumentLabellingMode(Semantics.COMPLETE)
        .orderingPrinciple(OrderingPrinciple.WEAKEST)
        .orderingComparator(OrderingComparator.DEMOCRAT)
        .graphExtensions(GraphExtension.STANDARD_PREF, GraphExtension.REBUT_RESTRICTION)   // none: no argument
        .autoTransposition(true)
```

| Flag | Values |
| --- | --- |
| `graphBuildMode` | `Profile`: `FREE`, `ASPIC_PLUS`, `ASPIC`, `ASPIC_DEFEASIBLE`, `META`, `ABA`, `ABA_PLUS`, `ABSTRACT` |
| `argumentLabellingMode` | `Semantics`: `GROUNDED`, `COMPLETE`, `PREFERRED`, `STABLE`, ... |
| `statementLabellingMode` | `StatementLabelling` |
| `orderingPrinciple`, `orderingComparator` | `OrderingPrinciple`, `OrderingComparator` |
| `graphExtensions`, `graphLanguage` | `GraphExtension`, `GraphLanguage` (several) |
| `graphBuilder`, `conflictModel` | `GraphBuilder`, `ConflictModel` |
| `modulesPath` | a path |
| `queryMode`, `autoTransposition`, `prologStrictCompatibility`, `metaConflicts`, `metaRules`, `debug` | on or off |

Each value has the atom stating it in a theory (`Semantics.PREFERRED.value` is `preferred`); `toString()` writes the
flags as the facts stating them. A flag the theory states overrides the one of the solver.

---

## Answers and graphs

`answer(query)` gives the statements for a query labelled in, out and und (`inStatements`, `outStatements`,
`undStatements`, as [values](#prolog-goals-and-values), and `label(statement)`).

`graphs()` gives the graph of each labelling of the theory: one, or several with a semantics admitting several
(preferred, stable, ...):

```kotlin
Arg2p.of(theory, Flags.defaults().argumentLabellingMode(Semantics.PREFERRED))
    .graphs()
    .forEachIndexed { index, graph ->
        println("Extension #$index: ${graph.conclusions("in")}")
    }
```

A theory its profile refuses (a non-flat ABA theory, say), or a query that cannot be parsed, raises an
`Arg2pException`, with the reason.

The context of a solver keeps a branch for every stage of a resolution (each module call made with `:::`), numbered
from 0: `activeBranch` is the last one, `graph()` and `graphAt(branch)` the graph of the last or of any of them, and
`contextFacts(branch, goal)` what a branch stores.

### The result model

A graph is made of plain data classes (package `it.unibo.tuprolog.argumentation.core.model`):

| Type | Content |
| --- | --- |
| `Graph` | `arguments`, `attacks`, `supports`, `labellings: List<LabelledArgument>`; `label(argument)`, `labelled(label)`, `conclusions(label)` |
| `LabelledArgument` | `argument: Argument` and its `label` (`in`, `out`, `und`, or `na` when unlabelled) |
| `Argument` | `identifier`, `rules`, `topRule`, `conclusion`, `supports` (its sub-arguments) and a readable `descriptor` |
| `Attack` | `attacker`, `target`, the attack `type` and the attacked sub-argument `on` |
| `Support` | `supporter` and `supported` |

Attack types are `rebut`, `contrary_rebut`, `undermine`, `contrary_undermine` and `undercut`, plus `pref` (an
attack of a preference, with `defeasiblePref` and `defeasibleAllPref`), `bprebut` (with `bp`) and `abstract` (an
attack of an abstract framework).

```kotlin
graph.attacks.forEach { println("${it.attacker.descriptor} --${it.type}--> ${it.target.descriptor}") }
```

Arguments receive synthetic identifiers (`A0`, `A1`, …) assigned in a stable order, so the same theory always
produces the same names.

---

## Prolog goals and values

`solve(goal)` gives the solutions of a Prolog goal, computed one at a time as they are asked for (`hasNext()` and
`next()`, a Kotlin `for`, or `toList(limit)`). A solution is yes, no or halt (`isYes`, `isNo`, `isHalt`, and the
`error` of a halt); a yes one binds the variables of the goal (`variables`, `bindings`, `value(name)`, `text(name)`).

The terms are `Value`s: `Value.Atom`, `Value.Integer`, `Value.Real`, `Value.Variable`, `Value.List` and
`Value.Struct`, data classes written as Arg2P writes them (`-a`, not `'-'(a)`). `Value.parse(text)` reads one.

```kotlin
for (solution in Arg2p.of().solve("member(X, [a, f(-b)])")) {
    if (solution.isYes) println(solution.value("X"))   // a, then f(-b)
}
```

---

## Causality

`causality` tests causality on the theory of a solver (see [causality]({{% ref "/docs/modules/causality" %}})):

```kotlin
val causality = Arg2p.of("r1 :=> a.\nr2 : a => b.").causality
causality.ness("a", "b")      // [-a], the intervention: null if a is not a NESS cause of b
causality.butFor("a", "b")    // true
```

`nessOriginal` is the NESS test as published, `nessIntervention` and `nessOriginalIntervention` whether a given
intervention meets the condition.

---

## The parallel solver

The clusters of the [parallel solver]({{% ref "/docs/modules/parallel-solver" %}}) take the same `Flags`:
`LocalCluster(parts, flags)` on every platform, `ThreadedCluster(threads, parts, flags)` on the JVM.

---

## Earlier entry points

`Arg2pSolverFactory.evaluate(theory, flags)` (package `it.unibo.tuprolog.argumentation.core`), the graph of each
labelling of a theory with the flags of a `FlagsBuilder`, is still there, as `FlagsBuilder` is, for the 2P-Kt
integration below: `Flags.of(builder)` are the flags of a builder.

---

## Building a solver by hand

Arg2P runs on its own engine, a fork of the one of 2P-Kt shipped with the core (`core`, `core-jvm` for the JVM):
`Arg2pPrologSolverFactory` builds its solvers, with the same methods as the `PrologSolverFactory` of 2P-Kt.
`Arg2pSolver.default()` builds the library set, and `to2pLibraries()` converts it into libraries for the solver:

```kotlin
import it.unibo.tuprolog.argumentation.core.Arg2pSolver
import it.unibo.tuprolog.argumentation.core.engine.Arg2pPrologSolverFactory
import it.unibo.tuprolog.argumentation.core.runtime.settings.FlagsBuilder
import it.unibo.tuprolog.solve.flags.FlagStore
import it.unibo.tuprolog.solve.flags.Unknown

val solver = Arg2pPrologSolverFactory.mutableSolverWithDefaultBuiltins(
    otherLibraries = Arg2pSolver.default(listOf(FlagsBuilder().create())).to2pLibraries(),
    flags = FlagStore.DEFAULT.set(Unknown, Unknown.FAIL),
)
```

The flags library (see [below](#adding-the-flags-library)) sets the flags of the evaluation; `Unknown.FAIL`
makes the hooks a theory may leave undefined (`conflict/2`, `sup/2`, ...) fail quietly, rather than with a warning
each time they are called.

The aggregate artifact `arg2p` (`arg2p-jvm` on the JVM), bundling the causality solver, has the same libraries with the
[causality]({{% ref "/docs/modules/causality" %}}) module as well: `Arg2pLibrary.get()` (package
`it.unibo.tuprolog.argumentation`), and `Arg2pLibrary.version()` is the version of Arg2P.

You can then use any predicate of the [Prolog interface]({{% ref "/docs/api/engine" %}}):

```kotlin
solver.solve(Struct.parse("arg2p::buildLabelSets(SIn, SOut, SUnd)", solver.operators)).first()
```

Pass `solver.operators` to `Struct.parse`: the module-call operator `::` is defined by Arg2P, and a goal using
it cannot be parsed without them.

### Parsing a theory

Arg2P adds operators (`=>`, `:=>`, `:->`, `:`) that a plain parser does not know, so theories must be parsed
with the Arg2P operator set. `RuleAnnotations.parse` (package `it.unibo.tuprolog.argumentation.core.runtime.theory`)
does it, and tags the clauses with the identifiers of their comments as well, which
[plain Prolog theories]({{% ref "/docs/syntax#theories-in-plain-prolog" %}}) need:

```kotlin
val theory = RuleAnnotations.parse(theoryText, Arg2pSolver.default().operators())
solver.loadStaticKb(theory)
```

### Adding the flags library

A hand-built solver has no flags unless the library set includes them, as above. They can also be added to
the libraries afterwards:

```kotlin
val settings = FlagsBuilder().create()
val solver = Arg2pPrologSolverFactory.mutableSolverWithDefaultBuiltins(
    otherLibraries = Arg2pSolver.default().to2pLibraries() + settings.content(),
    flags = FlagStore.DEFAULT.set(Unknown, Unknown.FAIL),
)
```

The libraries also run on the solver of 2P-Kt (`PrologSolverFactory`, from its `solve-prolog` package, not a
dependency of Arg2P), where module calls go through a slower path. That solver loses bindings a goal left to
solve still needs (a bug the engine of Arg2P fixes), unless it tracks every variable: set
`TrackVariables.ON` in its flags, besides `Unknown.FAIL`.

---

## Mining results from a solver

After running a query on your own solver, the computed graph can be read back as Kotlin objects with the
`graph()` extension:

```kotlin
import it.unibo.tuprolog.argumentation.core.model.mining.graph

solver.solve(Struct.parse("arg2p::solve", solver.operators)).first()
val graph = solver.graph()
```

`graph()` reads the *currently active* evaluation context, empty until a query has built a graph, so always
run a query first. Finer-grained accessors — `arguments()`, `attacks()`, `supports()`, `labels()` — are
available for a specific context id.

---

## Writing goals with the DSL

`arg2pScope` gives a typed builder for Prolog goals, including the `::` module-call operator:

```kotlin
import it.unibo.tuprolog.argumentation.core.dsl.arg2pScope

arg2pScope {
    solver.solve("abstract" call "solve"(listOf("a", "b"), listOf("a" to "b"), I, O, U))
}
```

See [Modules]({{% ref "/docs/modules" %}}) for what the module prefix means and which modules exist.
