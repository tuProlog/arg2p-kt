---
title: JavaScript
weight: 35
aliases:
  - /docs/javascript/
---

## JavaScript API

The npm package `@tuprolog/arg2p` exposes arg2p to JavaScript through the `js` packages of the core and of the
causality solver. The solvers, their solutions and the terms are tuProlog's own; the `js` packages export what
JavaScript cannot reach by itself: building a solver, reading its solutions one at a time, the argumentation graph
it computed, and building and reading terms.

```json
{
  "dependencies": {
    "@tuprolog/arg2p": "{{< version >}}"
  }
}
```

```js
const argumentation = require('@tuprolog/arg2p').it.unibo.tuprolog.argumentation
const { JsArg2p, JsSolverBuilder, JsTerms, JsTheories } = argumentation.core.js
const { JsCausality } = argumentation.causality.js
```

`argumentation.Arg2pLibrary.version()` is the version of the package.

---

## A solver

`JsSolverBuilder` describes a solver, then `build()` makes it:

| Method | Meaning |
| --- | --- |
| `theory(text)` | The theory, as Prolog text |
| `flags(text)` | The [flags]({{% ref "/docs/api/flags" %}}), as Prolog facts: the solver has only these, not the default ones |
| `module(module)` | A module besides those of arg2p, as the causality solver's `JsCausality.module()` |
| `output(consumer)` | A function receiving whatever the solver prints (the standard output otherwise) |

Without `flags`, the solver has the default flags, and the theory states those that differ (as `queryMode.`).
With `flags`, the string must declare every required flag.

```js
const solver = new JsSolverBuilder()
    .theory(`
        f1 :=> d.
        f2 :=> -d.`)
    .flags(`
        graphBuildMode(standard_af).
        statementLabellingMode(statement).
        argumentLabellingMode(grounded).
        orderingPrinciple(last).
        orderingComparator(elitist).
        graphExtension(standardPref).
        queryMode.`)
    .output(out => console.log(out))
    .build()
```

`JsArg2p.solver(theory)` and `JsArg2p.solverWithoutDefaults(theory)` are the short forms for a theory with the default
flags, or with only those it states.

---

## Its solutions

`JsArg2p.solve(solver, goal)` gives the solutions of a goal, computed one at a time as they are asked for, with
`hasNext()` and `next()`. Each solution has:

| Member | Content |
| --- | --- |
| `isYes`, `isNo`, `isHalt` | What the solution is (halt: the solver raised an error) |
| `query` | The goal, with its variables bound for a yes solution |
| `error` | The error of a halt solution, as text |
| `variables` | The names of the variables a yes solution binds |
| `valueOf(name)`, `formatted(name)` | The value of a variable, as a term or as text |

```js
const solution = JsArg2p.solve(solver, 'answerQuery(d, In, Out, Und)').next()
console.log(solution.formatted('Und'))     // [d]
```

---

## The argumentation graph

After a query, the context of the solver holds the graph it computed. `JsArg2p.graph(solver)` gives it, with:

| Field | Content |
| --- | --- |
| `arguments` | Objects with `id`, `descriptor` and `label` (`in`, `out`, `und`) |
| `attacks` | Objects with `from` and `to`, the ids of the two arguments |

```js
JsArg2p.solve(solver, 'buildLabelSets').next()
const graph = JsArg2p.graph(solver)

graph.arguments.forEach(arg => console.log(`${arg.label} : ${arg.descriptor}`))
graph.attacks.forEach(att => console.log(`${att.from} attacks ${att.to}`))
```

The context keeps a branch for every stage of the computation (each module call made with `:::`), numbered from 0:
`JsArg2p.activeBranch(solver)` is the last one, `JsArg2p.graphAt(solver, branch)` the graph of any of them, and
`JsArg2p.contextFacts(solver, branch, goal)` the solutions of `context_check(Branch, Goal)`, what a branch stores:

```js
const facts = JsArg2p.contextFacts(solver, JsArg2p.activeBranch(solver), 'statIn(S)')
while (facts.hasNext()) console.log(facts.next().formatted('S'))
```

---

## Causality

The causality predicates come with the causality module, given to the solver:

```js
const causal = new JsSolverBuilder()
    .theory(`
        r1 :=> a.
        r2 : a => b.`)
    .module(JsCausality.module())
    .build()

console.log(JsArg2p.solve(causal, 'causality::ness(X, a, b)').next().formatted('X'))   // [- a]
```

---

## Terms and theories

- `JsTerms`: parsing and formatting terms (`parse`, `format`), building them (`atom`, `variable`, `integer`, `real`,
  `struct`, `list`), and reading them (`kind`, `functor`, `arity`, `args`, `items`, `name`, `numberValue`);
- `JsTheories`: parsing a theory, its clauses, and formatting it.

The solver is tuProlog's own, so its named methods work as well:

```js
solver.assertAFact(JsTerms.struct('fact', [JsTerms.atom('x')]))
```

---

## From the JS bridge

Earlier versions of the package exposed a single object, `JsBridge`, solving a query on a theory and flags given as
text. Its replacements:

| `JsBridge` | Now |
| --- | --- |
| `JsBridge.solve(query, theory, flags, output)` | `new JsSolverBuilder().theory(theory).flags(flags).output(output).build()`, then `JsArg2p.solve(solver, query)` |
| the causality library, always there | `.module(JsCausality.module())` |
| `result.i.hasNext()`, `result.i.next()` | `solutions.hasNext()`, `solutions.next()` |
| `solution.res` | `solution.isYes`, `isNo`, `isHalt` |
| `solution.substitutions` | `solution.variables` and `solution.formatted(name)` |
| `solution.exception` | `solution.error` |
| `solution.graph`, `solution.context` | `JsArg2p.graph(solver)`, `JsArg2p.activeBranch(solver)` |
| `result.graphAt(branch)` | `JsArg2p.graphAt(solver, branch)` |
| `result.queryContext(branch, goal)` | `JsArg2p.contextFacts(solver, branch, goal)` |

---

## Full example

A complete browser application — the Web Playground — is available at
[tuprolog/arg2p-kt-web](https://github.com/tuProlog/arg2p-kt-web), and runs online
[here](https://tuprolog.github.io/arg2p-kt-web/).
