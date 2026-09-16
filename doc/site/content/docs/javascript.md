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
reachable through the bridge — see [Causality]({{% ref "/docs/causality" %}}).

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
