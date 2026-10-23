---
title: Kotlin
weight: 30
aliases:
  - /docs/kotlin-api/
---

## Kotlin API

There are two ways to use Arg2P from Kotlin or Java:

1. **The high-level API** — `Arg2pSolverFactory.evaluate`, which runs a theory and hands back the resulting
   graph as Kotlin objects. Start here.
2. **The 2P-Kt integration** — load Arg2P as a library into your own `Solver` and query it with Prolog goals.
   Use this when you need the full Prolog interface, custom libraries, or an existing 2P-Kt setup.

---

## The high-level API

`Arg2pSolverFactory.evaluate(theory, flags)` evaluates a theory and returns a sequence of
`Graph` objects — one per labelling, since some semantics admit more than one.

```kotlin
import it.unibo.tuprolog.argumentation.core.Arg2pSolverFactory
import it.unibo.tuprolog.argumentation.core.runtime.settings.FlagsBuilder

fun main() {
    val graph = Arg2pSolverFactory.evaluate(
        """
        f1 :=> d.
        f2 :=> -d.
        """.trimIndent(),
        FlagsBuilder(),
    ).first()

    graph.labellings.forEach {
        println("${it.label} : ${it.argument.conclusion}")
    }
}
```

### Configuring the evaluation

Every [flag]({{% ref "/docs/api/flags" %}}) is a field of `FlagsBuilder`, so it can be set with named arguments:

```kotlin
val flags = FlagsBuilder(
    argumentLabellingMode = "complete",
    statementLabellingMode = "statement",
    orderingPrinciple = "weakest",
    orderingComparator = "democrat",
    graphExtensions = listOf("standardPref", "rebutRestriction"),
    autoTransposition = true,
)
```

Most fields also have a fluent setter, which is handy when adjusting a single value:

```kotlin
val flags = FlagsBuilder().argumentLabellingMode("preferred")
```

> [!NOTE]
> `autoTransposition` has no fluent setter — set it through the constructor, or with `copy()` on an existing
> builder.

### Reading several labellings

With a multiple-status semantics, do not stop at `first()`:

```kotlin
Arg2pSolverFactory
    .evaluate(theory, FlagsBuilder(argumentLabellingMode = "preferred"))
    .forEachIndexed { index, graph ->
        println("Extension #$index")
        graph.labellings.forEach { println("  ${it.label} : ${it.argument.conclusion}") }
    }
```

---

## The result model

`evaluate` returns `Graph` objects made of plain data classes:

| Type | Content |
| --- | --- |
| `Graph` | `labellings: List<LabelledArgument>`, `attacks: List<Attack>`, `supports: List<Support>` |
| `LabelledArgument` | `argument: Argument` and its `label` (`in`, `out`, `und`, or `na` when unlabelled) |
| `Argument` | `rules`, `topRule`, `conclusion`, `supports` (its sub-arguments) and a readable `descriptor` |
| `Attack` | `attacker`, `target`, the attack `type` and the attacked element `on` |
| `Support` | `supporter` and `supported` |

Attack types are `rebut`, `contrary_rebut`, `undermine`, `contrary_undermine` and `undercut`.

```kotlin
graph.attacks.forEach { println("${it.attacker.descriptor} --${it.type}--> ${it.target.descriptor}") }
```

Arguments receive synthetic identifiers (`A0`, `A1`, …) assigned in a stable order, so the same theory always
produces the same names.

---

## Building a solver by hand

Arg2P runs on its own engine, a fork of the one of 2P-Kt shipped with the core (`core`, `core-jvm` for the JVM):
`Arg2pPrologSolverFactory` builds its solvers, with the same methods as the `PrologSolverFactory` of 2P-Kt.
`Arg2pSolver.default()` builds the library set, and `to2pLibraries()` converts it into libraries for the solver:

```kotlin
import it.unibo.tuprolog.argumentation.core.Arg2pSolver
import it.unibo.tuprolog.argumentation.core.engine.Arg2pPrologSolverFactory

val solver = Arg2pPrologSolverFactory.solverWithDefaultBuiltins(
    otherLibraries = Arg2pSolver.default().to2pLibraries(),
)
```

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
with the Arg2P operator set:

```kotlin
val theory = Theory.parse(theoryText, Arg2pSolver.default().operators())
```

### Adding the flags library

A hand-built solver has no flags. Add them as a library:

```kotlin
val settings = FlagsBuilder().create()
val solver = Arg2pPrologSolverFactory.mutableSolverWithDefaultBuiltins(
    otherLibraries = Arg2pSolver.default().to2pLibraries() + settings.content(),
)
```

The libraries also run on the solver of 2P-Kt (`PrologSolverFactory`, from its `solve-prolog` package, not a
dependency of Arg2P), where module calls go through a slower path.

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
