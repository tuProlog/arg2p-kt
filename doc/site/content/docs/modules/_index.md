---
title: Modules
weight: 25
bookCollapseSection: true
---

## Modules

Arg2P is not a single monolithic theory: it is a set of **modules**, each providing one step of the
argumentation pipeline. Modules are loaded on demand, which keeps memory usage low and makes the behaviour of
the engine configurable — choosing a semantics, for instance, simply means dispatching to a different module.

You normally never call a module directly: the [engine interface]({{% ref "/docs/predicate" %}}) does it for
you, guided by the [flags]({{% ref "/docs/flags" %}}). Calling one explicitly is useful when debugging, when
running a single stage of the pipeline, or when extending the framework.

The modular model implemented here — fragmenting a theory into modules that coexist, interact and nest — is
described in Calegari, Contissa, Pisano, Sartor and Sartor, _Modular logic argumentation in Arg-tuProlog_; see
[References]({{% ref "/docs/references" %}}).

---

## Calling a module

Two operators dispatch a goal to a module:

```prolog
Module::Goal       % run Goal inside Module
Module:::Goal      % same, but in a branched evaluation context
```

For example, to run the abstract solver directly:

```prolog
?- abstract::solve([a, b, c], [(a, b), (b, c)], I, O, U).
```

### `::` versus `:::`

The difference matters when a stage can produce **more than one result**.

- `::` runs the goal in the current context. Whatever the goal asserts stays in that context.
- `:::` **branches** the context first, so each solution gets its own private copy of the computed data.

This is how multi-extension semantics work: `preferred`, `stable`, `ideal`, `cf2` and `stage2` enumerate their
extensions by branching with `:::`, each branch holding one labelling. When you iterate over the solutions of
such a query, you are iterating over those branches.

---

## The pipeline

A complete evaluation runs these stages in order. Each one is a module, selected by a flag where more than one
implementation exists.

| Stage | Module | Selected by |
| --- | --- | --- |
| Parse the theory into internal rules | `parser` | always |
| Build arguments, attacks and supports | `standard_af` | `graphBuildMode` |
| Apply graph extensions (preferences, restrictions) | one per extension | `graphExtension` |
| Label the arguments | one labeller per semantics | `argumentLabellingMode` |
| Label the statements | `statement`, `statement_pass_through`, `statement_binary` | `statementLabellingMode` |

The `abstract` module orchestrates the whole sequence, and `structured` implements the goal-directed variant
used when `queryMode` is enabled.

---

## Available modules

### Pipeline modules

| Module | Purpose |
| --- | --- |
| `abstract` | Orchestrates the evaluation; also exposes the [abstract AF entry points]({{% ref "/docs/modules/abstract" %}}). |
| `structured` | Goal-directed evaluation used by `answerQuery`. |
| `parser` | Translates the Arg2P language into the engine's internal rule format. |
| `standard_af` | Builds the argumentation graph. |
| `attack` | Defines attack and conflict relations. |
| `superiority` | Implements argument ordering and comparators. |
| `utils` | List, sorting and hashing helpers. |
| `debug` | Pretty-prints theories, graphs and labellings. |
| `interpreter` | Meta-interpreter, used to trace how facts were derived. |
| `module` | Loads external `.pl` files, see [external modules](#external-modules). |

### Labelling modules

One per value of `argumentLabellingMode` (`grounded`, `complete`, `preferred`, `stable`, `bp_grounded`, …) and
one per value of `statementLabellingMode`. They all expose the same entry point, `argumentLabelling/0` or
`statementLabelling/0` respectively, which is what makes them interchangeable:

```prolog
?- grounded::argumentLabelling.
?- statement::statementLabelling.
```

### Graph extension modules

`standardPref`, `defeasiblePref`, `defeasibleAllPref`, `rebutRestriction` and `bp` all expose
`modifyArgumentationGraph/0`, and are applied after the graph has been built.

---

## Inspecting intermediate results

Data produced by a stage is stored in the evaluation context and can be read back with `context_check/1`:

```prolog
?- context_check(argument(A)).
?- context_check(attack(Type, Attacker, Attacked, On)).
?- context_check(in(A)).
```

This is how the Kotlin [`graph()`]({{% ref "/docs/kotlin-api" %}}) helper reads results, and it is the most
direct way to see what a single stage produced.

Useful context predicates:

| Predicate | Content |
| --- | --- |
| `argument/1` | An argument built by the graph builder. |
| `attack/4` | `attack(Type, Attacker, Attacked, On)`. |
| `support/2` | `support(Supporter, Supported)`. |
| `in/1`, `out/1`, `und/1` | The argument labelling. |
| `statIn/1`, `statOut/1`, `statUnd/1` | The statement labelling. |
| `sup/2` | Superiority relations declared in the theory. |

---

## External modules

Theories kept in separate `.pl` files can be loaded and queried without adding them to the knowledge base:

```prolog
module::call_module([Modules], Goal)
```

`Modules` is a list of module names, resolved against the
[`modulesPath`]({{% ref "/docs/flags" %}}) location, and `Goal` is solved against their combined content.
A full location can be given instead of a name, in which case `modulesPath` is ignored:

```prolog
?- module::call_module(['greetings'], hello(X)).
?- module::call_module(['/home/me/theories/greetings.pl'], hello(X)).
```

### Remote modules

Modules are resolved as URLs, so they do not have to live on the local filesystem. `modulesPath` may be a base
URL, and a module may be given as a full URL:

```prolog
modulesPath('https://example.org/theories').

?- module::call_module(['greetings'], hello(X)).
?- module::call_module(['https://example.org/theories/greetings.pl'], hello(X)).
```

A URL does not need to end in `.pl`: anything carrying a scheme is treated as a location rather than as a
module name.

> [!WARNING]
> A remote module is fetched and loaded as part of your theory, so it is executed with the same trust as the
> rules you wrote yourself. Only point `modulesPath` at a location you control, and prefer `https` — content
> fetched over plain `http` can be substituted in transit.

The modules are loaded into a separate solver, so their clauses do not pollute the theory under evaluation.
A module that cannot be found makes the call fail, so check the path when a goal you expect to hold does not
succeed.

### What a module sees

A module may be written in plain Prolog or in the [Arg2P language]({{% ref "/docs/syntax" %}}), and a full
evaluation can be run over it, which makes `call_module/2` a way to reason about a theory kept in a separate
file:

```prolog
?- module::call_module(['theory'], arg2p::solve(d, Res)).
Res = [in(d)]
```

Inside the call:

- the [flags]({{% ref "/docs/flags" %}}) of the calling solver apply, so the module is evaluated under the
  same semantics, ordering and preferences;
- the theory of the calling solver is **not** visible: the module is evaluated on its own content alone;
- the resulting graph is written to the usual evaluation context, so it can be
  [mined]({{% ref "/docs/kotlin-api" %}}) afterwards and is displayed in the IDE's _Graph_ tab like any other
  evaluation.

> [!NOTE]
> Evaluating a module replaces the current context, so the graph you inspect afterwards is the module's, not
> the one built from the theory loaded in the solver.

---

## Adding your own module

A module is an `ArgLibrary` whose `identifier()` is the name used after `::`. Pass your libraries to the
factory to have them loaded alongside the built-in ones:

```kotlin
val solver = Arg2pSolverFactory.default(
    theory = theory,
    staticLibs = listOf(MyLibrary()),   // always loaded
    dynamicLibs = listOf(MyModule()),   // loaded on first use
)
```

Static libraries are materialised immediately; dynamic ones are resolved the first time a `::` or `:::` call
mentions them. The [causality]({{% ref "/docs/modules/causality" %}}) and actor solvers are built exactly this way.
