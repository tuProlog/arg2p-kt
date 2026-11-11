---
title: Modules
weight: 25
bookCollapseSection: true
---

## Modules

Arg2P is not a single monolithic theory: it is a set of **modules**, each providing one step of the
argumentation pipeline. Modules are loaded on demand, which keeps memory usage low and makes the behaviour of
the engine configurable — choosing a semantics, for instance, simply means dispatching to a different module.

You normally never call a module directly: the [engine interface]({{% ref "/docs/api/engine" %}}) does it for
you, guided by the [flags]({{% ref "/docs/api/flags" %}}). Calling one explicitly is useful when debugging, when
running a single stage of the pipeline, or when extending the framework.

The modular model implemented here — fragmenting a theory into modules that coexist, interact and nest — is
described in Calegari, Contissa, Pisano, Sartor and Sartor, _Modular logic argumentation in Arg-tuProlog_; see
[References]({{% ref "/docs/references" %}}).

---

## How a module is laid out

In the sources, every module is a directory named after it, with the same shape:

```text
core/libraries/labelling/argument/grounded/
  GroundedLibrary.kt     its name and what it registers
  prolog/theory.pl      its Prolog theory, if any
  primitives/           the primitives written in Kotlin, if any
  language/             what it reads of a theory, if any: language/theory.pl, its primitives and operators
  flags/                the flags it declares, if any
  helpers/              the Kotlin code those rely on, if any
```

The theories are plain Prolog files in the common sources. The build compiles them into constants
(`embedPrologTheories`), as browsers have no files to read when the solver runs: every platform runs the same text.
They use the operators of the modules (`::`, `:::`), which only the parser of Arg2P reads.

The libraries under `core/libraries/framework` are those the others stand on. The loader of the modules (`::`, `:::`),
the context, the cache and the settings are there from the start in every solver, each with a state of its own; the
parser, the store and utils, beside them, are modules like the others, loaded by their first call. A theory goes in through
the parser, which reads it as the extensions of its language give it (the notation of the rules, the languages of the
profile, the graph extensions), and the store keeps what is read and serves it to the others (`store::rule/3`,
`store::premise/2`, ...), as it keeps the graph. The modules are grouped by what
they do, each using only its own area and those below it:

```text
core/libraries/
  framework/   loader, context, cache, settings; parser (how a theory goes in); store (the rules read, the graph and its
               labels, kept in the cache and the context, and the terms they are made of), utils
  language/    the languages of a theory (ruleNotation, aspicLanguage, abaLanguage, plainProlog, temporal, transposition,
               metaRules, ...), conflict (which statements conflict), moduleCalls (external modules)
  graph/       structured/ (the graphs built from rules: ruleGraph, queryGraph, argument/, attack/, extension/ with
               the preference modes and superiority), abstractGraph
  labelling/   the semantics, of the arguments and of the statements
  evaluation/  pipeline, statementAcceptance
  arg2p/       the entry points, and the printing of the graph and its labels (buildLabelSets/0)
```

---

## Calling a module

Two operators dispatch a goal to a module:

```prolog
Module::Goal       % run Goal inside Module
Module:::Goal      % same, but in a branched evaluation context
```

For example, to read the rules of the theory as the store keeps them:

```prolog
?- store::rule(Id, Premises, Conclusion).
```

### `::` versus `:::`

The difference matters when a stage can produce **more than one result**.

- `::` runs the goal in the current context. Whatever the goal asserts stays in that context.
- `:::` **branches** the context first, so each solution gets its own private copy of the computed data.

This is how multi-extension semantics work: `preferred`, `stable`, `cf2` and `stage2` enumerate their
extensions by branching with `:::`, each branch holding one labelling. When you iterate over the solutions of
such a query, you are iterating over those branches.

### The theory is the module `user`

The theory you write is a module of its own, `user`, and its predicates are its own: the modules of Arg2P keep
theirs inside, so a theory can define any predicate (`member/2`, `holds/1`, `contrary/2`, ...) without changing
how the engine works, and a predicate the theory defines is the one its goals call, whatever a library names the
same way. The modules reach the theory only through `user::Goal`, the way they read its rules, preferences and
conflicts, and run its `prolog/1` checks:

```prolog
?- user::findall(X, member(X, [a, b]), L).
```

---

## The pipeline

A complete evaluation runs these stages in order. Each one is a module, selected by a flag where more than one
implementation exists.

| Stage | Module | Selected by |
| --- | --- | --- |
| Parse the theory into internal rules, in the languages of its profile, rewritten once by the extensions defining `rewrite/4` | `parser`, `aspicLanguage`, `abaLanguage`, `defeasibleLanguage`, `abstractLanguage`, `naturalTerms`, `metaRules` | `graphLanguage` (set by `graphBuildMode`), `naturalTerms`, `metaRules` |
| Build arguments, attacks and supports | `ruleGraph`, `abstractGraph` | `graphBuilder` (set by `graphBuildMode`) |
| Change the graph, in a fixed order: add the arguments derived from the others (the copies of `metaConflicts`) with their attacks, then `rebutRestriction`, the preference mode, `bp` | `conflictArguments`, one per extension | `graphExtension`, `metaConflicts` |
| Label the arguments | one labeller per semantics | `argumentLabellingMode` |
| Label the statements | `statement`, `statementPassThrough`, `statementBinary` | `statementLabellingMode` |

The `pipeline` module runs the stages: `pipeline::evaluate/0` on the whole graph, `pipeline::evaluate/1` on the part of
the graph a goal depends on when `queryMode` is enabled and the semantics allows it (`pipeline::locality/1`), which is
what `statementAcceptance` uses. The two differ in the second stage alone, which the builder gives: the whole graph
(`buildArgumentationGraph/0`), or the part the query depends on (`buildQueryGraph/3` of `ruleGraph`, through
`queryGraph`), first through the arguments alone (their premises and possible attackers, and for a connected
semantics what they attack and support), then through what the steps changing the graph relate them to
(`queryDependencies/2` and `queryDependants/2` of the modules defining them), the attacks between them after. The other
stages are the same, and so are the stages of the context the evaluation leaves (see `graphAt`).

---

## Available modules

### Pipeline modules

Every module, with all its predicates, is in the [Prolog modules reference]({{% ref "/docs/api/prolog" %}}).

| Module | Purpose |
| --- | --- |
| `pipeline` | The evaluation: the profile checked, the graph built (whole, or the part a goal depends on), the steps changing it, the labellings. |
| `statementAcceptance` | The acceptance of a statement, used by `solve`: on the graph `queryMode` selects, as `answerMode` states. |
| `parser` | Reads the rules and premises of a theory, as the extensions of its language give them, and finds the extension extending a module (`hook/2`). |
| `store` | Keeps the rules read, the first time they are asked for, and the argumentation graph and its labels: the only module reading and writing them. |
| `ruleNotation` | The notation of the rules: `r : a => b`, `r : a -> b`, `f :=> a`, `f :-> a`. |
| `ruleGraph` | Builds the argumentation graph from the rules: its arguments (`argumentBuilder`) and the attacks between them (`attackBuilder`). Those derived from them are added when the graph is changed (`conflictArguments`). |
| `abstractGraph` | Builds the graph from the arguments and attacks a theory of the profile `abstract` states. |
| `conflict` | Which statements conflict, as the conflict model and the theory state it. |
| `attackRelation`, `attackSearch` | Define when an argument attacks another, and which arguments may. |
| `superiority` | The preferences between rules and between arguments the theory states, with their orderings: what the preference modes and the `bp` labellings compare. |
| `utils` | List and sorting helpers. |
| `module` | Loads external `.pl` files, see [external modules](#external-modules). |
| `metaRules` | With the flag `metaRules`, gives each rule `r` the premise `applicable(r)`: the theory argues about which of its rules apply. |

### Labelling modules

One per value of `argumentLabellingMode` (`grounded`, `complete`, `preferred`, `stable`, `bpGrounded`, …) and
one per value of `statementLabellingMode`. They all expose the same entry point, `argumentLabelling/0` or
`statementLabelling/0` respectively, which is what makes them interchangeable (`labelling` and `scc` hold what the
semantics share, and are no semantics themselves):

```prolog
?- grounded::argumentLabelling.
?- statement::statementLabelling.
```

### Graph extension modules

`standardPref`, `defeasiblePref`, `defeasibleAllPref`, `rebutRestriction`, `bp` and `abaPref` all expose
`modifyArgumentationGraph/0`, and are applied after the graph has been built: `conflictArguments` first with
`metaConflicts`, as it adds arguments whose attacks the others change.

---

## Inspecting intermediate results

Data produced by a stage is stored in the evaluation context and can be read back with `contextCheck/1`:

```prolog
?- contextCheck(argument(A)).
?- contextCheck(attack(Type, Attacker, Attacked, On)).
?- contextCheck(in(A)).
```

This is the most direct way to see what a single stage produced. The context holds the attacks as they are found,
each on the argument it targets: an attack on an argument also attacks every argument built on it, on the same
sub-argument, and `store::attack/4` reads them all, as the Kotlin [`graph()`]({{% ref "/docs/api/kotlin" %}})
helper does:

```prolog
?- store::attack(Type, Attacker, Attacked, On).
```

Useful context predicates:

| Predicate | Content |
| --- | --- |
| `argument/1` | An argument built by the graph builder. |
| `attack/4` | `attack(Type, Attacker, Attacked, On)`, as found (see `store::attack/4` for all of them). |
| `support/2` | `support(Supporter, Supported)`. |
| `in/1`, `out/1`, `und/1` | The argument labelling. |
| `statIn/1`, `statOut/1`, `statUnd/1` | The statement labelling. |
| `sup/2` | Superiority relations declared in the theory. |

---

## External modules

Theories kept in separate `.pl` files can be loaded and queried without adding them to the knowledge base:

```prolog
module::callModule([Modules], Goal)
```

`Modules` is a list of module names, resolved against the
[`modulesPath`]({{% ref "/docs/api/flags" %}}) location, and `Goal` is solved against their combined content.
A full location can be given instead of a name, in which case `modulesPath` is ignored:

```prolog
?- module::callModule(['greetings'], hello(X)).
?- module::callModule(['/home/me/theories/greetings.pl'], hello(X)).
```

### Remote modules

Modules are resolved as URLs, so they do not have to live on the local filesystem. `modulesPath` may be a base
URL, and a module may be given as a full URL:

```prolog
modulesPath('https://example.org/theories').

?- module::callModule(['greetings'], hello(X)).
?- module::callModule(['https://example.org/theories/greetings.pl'], hello(X)).
```

A URL does not need to end in `.pl`: anything carrying a scheme (`https://...`) is treated as a location rather than
as a module name, and so is a path ending in `.pl`. With no `modulesPath`, module names are resolved in the working
directory.

> [!WARNING]
> A remote module is fetched and loaded as part of your theory, so it is executed with the same trust as the
> rules you wrote yourself. Only point `modulesPath` at a location you control, and prefer `https` — content
> fetched over plain `http` can be substituted in transit.

The modules are loaded into a separate solver, so their clauses do not pollute the theory under evaluation.
A module is read with the operators of Arg2P (`::` and `:::` included). A module that cannot be found or read raises
an existence error naming it and saying why, one that does not parse a syntax error; an error raised within the module
is raised as it is.

### What a module sees

A module may be written in plain Prolog or in the [Arg2P language]({{% ref "/docs/syntax" %}}), and a full
evaluation can be run over it, which makes `callModule/2` a way to reason about a theory kept in a separate
file:

```prolog
?- module::callModule(['theory'], arg2p::solve(d, Res)).
Res = [in(d)]
```

Inside the call:

- the [flags]({{% ref "/docs/api/flags" %}}) the calling solver was given apply (those of its `Flags`, not
  those written in its theory), so the module is evaluated under the same semantics, ordering and preferences,
  unless the module states its own;
- the theory of the calling solver is **not** visible: the module is evaluated on its own content alone;
- the resulting graph is written to the usual evaluation context, so it can be
  [mined]({{% ref "/docs/api/kotlin" %}}) afterwards and is drawn by the solver of the playground like any other
  evaluation.

> [!NOTE]
> Evaluating a module replaces the current context, so the graph you inspect afterwards is the module's, not
> the one built from the theory loaded in the solver.

---

## Adding your own module

A module is an `ArgLibrary` that is also `Loadable`, as every `PrologLibrary`: its `identifier()` is the name
used after `::`. Pass your libraries to the
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
