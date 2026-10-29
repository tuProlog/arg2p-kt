---
title: JavaScript
weight: 35
aliases:
  - /docs/javascript/
---

## JavaScript API

The npm package `@tuprolog/arg2p` exposes the [API of arg2p]({{% ref "/docs/api/kotlin" %}}) to JavaScript, as it is
in Kotlin: a solver for a theory (`Arg2p`), its flags (`Flags`, with an enumeration for the values of each), its
answers, graphs and solutions, the values of Prolog terms (`Value`), the causality tests, and the clusters of the
[parallel solver]({{% ref "/docs/modules/parallel-solver" %}}). Nothing of tuProlog is needed.

```json
{
  "dependencies": {
    "@tuprolog/arg2p": "{{< version >}}"
  }
}
```

```js
import { Arg2p, Flags, Value, Semantics, Profile, GraphExtension } from '@tuprolog/arg2p'
```

The package is an ES module (Node.js, bundlers, Deno, browsers), every declaration of the API exported by its name.

`Arg2p.version()` is the version of the package. Every list of the API is an array in JavaScript.
The package declares its API for TypeScript (`arg2p.d.ts`): the classes, their methods and the enumerations are typed
in the editor.

---

## A solver

`Arg2p.of(theory, flags)` is a solver for a theory, Prolog text, with flags (`Flags.defaults()` if left out).
`Arg2p.builder()` describes one in full:

| Method | Meaning |
| --- | --- |
| `theory(text)` | The theory, as Prolog text |
| `flags(flags)` | The [flags]({{% ref "/docs/api/flags" %}}) |
| `output(consumer)` | A function receiving whatever the solver prints (the standard output otherwise) |

```js
const solver = Arg2p.builder()
    .theory(`
        f1 :=> d.
        f2 :=> -d.`)
    .flags(Flags.defaults().argumentLabellingMode(Semantics.PREFERRED))
    .output(out => console.log(out))
    .build()
```

`Flags.defaults()` are the flags of arg2p by default, `Flags.none()` only those the theory states, and
`Flags.parse(text)` those stated by Prolog facts. Each flag has a method, taking a value of its enumeration
(`Profile`, `Semantics`, `StatementLabelling`, `OrderingPrinciple`, `OrderingComparator`, `GraphExtension`,
`GraphLanguage`, `GraphBuilder`, `ConflictModel`), a boolean for a switch:

```js
Flags.defaults()
    .graphBuildMode(Profile.ABA)
    .graphExtensions([GraphExtension.STANDARD_PREF, GraphExtension.BP])   // [] for none
    .queryMode(false)
```

---

## Answers and graphs

`answer(query)` gives the statements for a query labelled in, out and und (`inStatements`, `outStatements`,
`undStatements`, and `label(statement)`). A theory its profile refuses, or a query that cannot be parsed, raises an
`Arg2pException`. When a text does not parse, its `syntax` tells where (`line`, `column`, `token`, `description`), and
`Arg2p.checkTheory(text)` and `Arg2p.checkQuery(text)` tell the same without solving anything (null for a text that
parses).

```js
const answer = Arg2p.of('r1 : [] => a.\nr2 : [] => -a.\nsup(r1, r2).').answer('a')
answer.inStatements.map(String)    // ['a']
```

`graphs()` gives the graph of each labelling of the theory: one, or several for a semantics with several (preferred,
stable, ...). A graph has its `arguments`, `attacks`, `supports` and `labellings` (each argument with its label), and
`label(argument)`, `labelled(label)` and `conclusions(label)`:

```js
const [graph] = solver.graphs()
graph.labellings.forEach(it => console.log(`${it.label} : ${it.argument.descriptor}`))
graph.attacks.forEach(it => console.log(`${it.attacker.identifier} attacks ${it.target.identifier} (${it.type})`))
graph.conclusions('in')
```

An argument has its `identifier` (A0, A1, ...), `descriptor` (`A2 : A0,r1 : c`), `conclusion`, `topRule`, `rules`
and `supports` (the arguments it is built on); an attack its `attacker`, `target`, `type` (`rebut`, `undermine`,
`undercut`, ...) and `on`, the sub-argument of the target it hits.

The context of a solver keeps a branch for every stage of a resolution (each module call made with `:::`), numbered
from 0: `activeBranch` is the last one, `graph()` and `graphAt(branch)` the graph of the last or of any of them, and
`contextFacts(branch, goal)` what a branch stores.

---

## Prolog goals

`solve(goal)` gives the solutions of a goal, computed one at a time as they are asked for, with `hasNext()` and
`next()` (or `toArray(limit)`). Each solution has:

| Member | Content |
| --- | --- |
| `isYes`, `isNo`, `isHalt` | What the solution is (halt: the solver raised an error) |
| `query` | The goal, with its variables bound for a yes solution |
| `error` | The error of a halt solution |
| `variables` | The names of the variables a yes solution binds |
| `value(name)`, `text(name)` | The value of a variable, as a `Value` or as Prolog text |

A `Value` is an atom (`name`), an integer or a real (`value`), a variable (`name`), a list (`items`) or a compound
term (`functor`, `args`); its `kind` tells which, and `toString()` writes it as arg2p does (`-a`). `Value.parse(text)`
reads one.

```js
const solutions = Arg2p.of().solve('member(X, [a, f(-b)])')
while (solutions.hasNext()) {
    const solution = solutions.next()
    if (solution.isYes) console.log(solution.text('X'), solution.value('X').kind)
}
```

`add(text)` adds clauses to the theory, `load(text)` replaces it.

---

## Causality

Every solver tests causality on its theory ([causality]({{% ref "/docs/modules/causality" %}})): `ness(cause, effect)`
and `nessOriginal(cause, effect)` give the intervention showing a cause NESS (null if it is not one),
`nessIntervention(intervention, effect)`, `nessOriginalIntervention(intervention, effect)` and `butFor(cause,
effect)` whether they hold.

```js
const causality = Arg2p.of('r1 :=> a.\nr2 : a => b.').causality
String(causality.ness('a', 'b'))   // [-a]
causality.butFor('a', 'b')         // true
```

---

## The parallel solver

The clusters of the [parallel solver]({{% ref "/docs/modules/parallel-solver" %}}) are solvers of arg2p as well: the
`solver` of a `WorkerCluster` (Node.js) or a `WebWorkerCluster` (browsers) solves any goal with the workers of the
cluster and of its federation, its solutions, answers and graphs promised in the types above; `stats` tells what the
federation does.

```js
const cluster = new WebWorkerCluster(4, '/arg2p-worker.js')
cluster.load(theory)
const solutions = await cluster.solver.solve('buildLabelSets(In, Out, Und)')
const solution = await solutions.next()
solution.text('In')                 // as for Arg2p
cluster.solver.graph()              // the Graph of the solution
cluster.stats.evaluation?.workers   // what each worker built
cluster.solver.stop()               // the goal being solved stopped: its next solution a halt
cluster.flags = Flags.defaults().queryMode(false)   // for the next evaluations
await cluster.connect('ws://hub-host:9431', '4831')  // presenting the code of an instance requiring one
```

---

## From earlier versions

Earlier versions exposed the solver of tuProlog through the `js` packages (`JsArg2p`, `JsSolverBuilder`, ...), and
before them a single object, `JsBridge`. Their replacements:

| Before | Now |
| --- | --- |
| `new JsSolverBuilder().theory(t).flags(f).output(o).build()` | `Arg2p.builder().theory(t).flags(Flags.parse(f)).output(o).build()` |
| `JsArg2p.solver(theory)`, `JsArg2p.solverWithoutDefaults(theory)` | `Arg2p.of(theory)`, `Arg2p.of(theory, Flags.none())` |
| `JsArg2p.solve(solver, goal)` | `solver.solve(goal)` |
| `solution.term(name)`, `solution.formatted(name)` | `solution.value(name)`, `solution.text(name)` |
| `JsArg2p.graph(solver)`, `JsArg2p.graphAt(solver, branch)` | `solver.graph()`, `solver.graphAt(branch)` |
| `graph.arguments[i].id`, `.label` | `graph.labellings[i].argument.identifier`, `graph.labellings[i].label` |
| `attack.from`, `attack.to`, `attack.on` (identifiers) | `attack.attacker`, `attack.target`, `attack.on` (arguments) |
| `JsArg2p.activeBranch(solver)`, `JsArg2p.contextFacts(solver, b, g)` | `solver.activeBranch`, `solver.contextFacts(b, g)` |
| `.module(JsCausality.module())`, then `causality::ness(X, a, b)` | `solver.causality.ness('a', 'b')` |
| `JsTerms.parse(text)`, `JsTerms.format(term)` | `Value.parse(text)`, `String(value)` |
| `require('@tuprolog/arg2p').it.unibo.tuprolog.argumentation...` | `import { ... } from '@tuprolog/arg2p'` |
| `JsBridge.solve(query, theory, flags, output)` | `Arg2p.builder()...build().solve(query)` |

---

## Full example

A complete browser application — the Web Playground — is available at
[tuprolog/arg2p-kt-web](https://github.com/tuProlog/arg2p-kt-web), and runs online
[here](https://tuprolog.github.io/arg2p-kt-web/).
