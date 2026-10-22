---
title: JavaScript API
weight: 17
---

## JavaScript API

The npm package `@tuprolog/arg2p` exposes the engine through a single object, `JsBridge`. It takes the query,
the theory and the flags as plain text, and returns the solutions together with the computed argumentation
graph.

```json
{
  "dependencies": {
    "@tuprolog/arg2p": "{{< version >}}"
  }
}
```

```js
const arg2p = require('@tuprolog/arg2p').it.unibo.tuprolog.argumentation.bridge.JsBridge
```

The package `@tuprolog/arg2p-causality-solver` is published as well, but the causality predicates are already
reachable through the bridge — see [Causality]({{% ref "/docs/modules/causality" %}}).

---

## `solve`

```js
JsBridge.solve(query, theory, flags, outputConsumer)
```

| Parameter | Meaning |
| --- | --- |
| `query` | The goal to run, e.g. `'arg2p::solve'` or `'arg2p::solve(d, Res)'` |
| `theory` | The theory, as Prolog text |
| `flags` | The [flags]({{% ref "/docs/flags" %}}), as Prolog facts |
| `outputConsumer` | A function receiving whatever the engine prints |

Unlike the Kotlin API, the bridge does **not** fill in default flags: the flags string must declare every
required one.

```js
const result = arg2p.solve('arg2p::solve', `
    f1 :=> d.
    f2 :=> -d.`, `
    graphBuildMode(standard_af).
    statementLabellingMode(statement).
    argumentLabellingMode(grounded).
    orderingPrinciple(last).
    orderingComparator(elitist).
    graphExtension(standardPref).
    queryMode.`, out => console.log(out))
```

---

## Reading the solutions

`solve` returns an object with an iterator `i` and the `query` that produced it. The iterator has `hasNext()`
and `next()`, and `next()` throws once the solutions are exhausted:

```js
const it = result.i

while (it.hasNext()) {
    const solution = it.next()
    console.log(solution.res)
}
```

### Solution fields

| Field | Content |
| --- | --- |
| `res` | `'yes'`, `'no'` or `'halt'` |
| `query` | The query this solution answers |
| `substitutions` | An array of `{ first, second }` pairs: variable name and its value |
| `exception` | The error message, when the engine raised one |
| `graph` | The argumentation graph computed for this solution |

### Graph fields

`solution.graph` has two arrays:

| Field | Content |
| --- | --- |
| `arguments` | Objects with `id`, `descriptor` and `label` (`in`, `out`, `und`) |
| `attacks` | Objects with `from` and `to`, the ids of the two arguments |

```js
const graph = result.i.next().graph

graph.arguments.forEach(arg => console.log(`${arg.label} : ${arg.descriptor}`))
graph.attacks.forEach(att => console.log(`${att.from} attacks ${att.to}`))
```

---

## Querying a single goal

```js
const solution = arg2p.solve('arg2p::solve(d, Res)', theory, flags, _ => { }).i.next()

solution.substitutions.forEach(s => console.log(`${s.first} = ${s.second}`))
```

---

## Full example

A complete browser application built on this bridge — the Web Playground — is available at
[tuprolog/arg2p-kt-web](https://github.com/tuProlog/arg2p-kt-web), and runs online
[here](https://tuprolog.github.io/arg2p-kt-web/).

---

## Without the bridge

tuProlog exports none of its declarations to JavaScript: its objects keep the names of their methods (`solveList`,
`assertAFact`, `loadStaticKb`, `castToStruct`, ...), but JavaScript cannot reach their factories, read their
properties or iterate a Kotlin sequence. The `js` package of `arg2p-core` exports what is missing, so that the
solvers, solutions and terms of tuProlog can be used directly:

- `JsTerms`: parsing and formatting terms, building them (`atom`, `variable`, `integer`, `real`, `struct`, `list`),
  and reading them (`kind`, `functor`, `arity`, `args`, `items`, `name`, `numberValue`);
- `JsTheories`: parsing a theory, and its clauses;
- `JsArg2p`: an arg2p solver for a theory (`solver`, with the default flags, or `solverWithoutDefaults`), and the
  solutions of a goal one at a time (`solve`, `solveTerm`), each with `isYes`, `isNo`, `isHalt`, `error`,
  `variables` and `valueOf(name)`.

```js
const { JsArg2p, JsTerms } = require('@tuprolog/arg2p').it.unibo.tuprolog.argumentation.core.js

const solver = JsArg2p.solver('r1 : [] => a. r2 : [] => -a. sup(r1, r2). queryMode.')
const solution = JsArg2p.solve(solver, 'answerQuery(a, In, Out, Und)').next()
console.log(JsTerms.format(solution.valueOf('In')))       // [a]

// solutions are computed as they are asked for
const lengths = JsArg2p.solve(solver, 'length(L, N)')
for (let i = 0; i < 3; i++) console.log(JsTerms.format(lengths.next().valueOf('N')))   // 0 1 2

// the solver is tuProlog's own
solver.assertAFact(JsTerms.struct('fact', [JsTerms.atom('x')]))
```
