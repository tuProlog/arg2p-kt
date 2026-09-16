---
title: Execution
weight: 10 
---

## JVM Library - Gradle

Arg2p is available as a [2P-Kt](https://github.com/tuProlog/2p-kt/) library.

To import the Arg2p module (version `{{< version >}}`) into your Kotlin-based Gradle project, declare the dependency in your `build.gradle(.kts)` file:
 ```kotlin
repositories {
    mavenCentral()
}

dependencies {
    implementation("it.unibo.tuprolog.argumentation:arg2p-jvm:{{< version >}}")
}
 ```

### Usage Example

```kotlin
import it.unibo.tuprolog.argumentation.core.Arg2pSolverFactory
import it.unibo.tuprolog.argumentation.core.libs.basic.FlagsBuilder

fun main() {
    val graph = Arg2pSolverFactory.evaluate("""
        f1 :=> d.
        f2 :=> -d.
    """.trimIndent(), FlagsBuilder()).first()

    graph.labellings.forEach {
        println("${it.label} : ${it.argument.conclusion}")
    }
}
``` 

For a complete example, check out the [GitHub demo](https://github.com/Gilbocc/arg2p-kt-demo).

### Available modules

`arg2p-jvm` is the aggregate module: it brings in the core engine and the causality solver, and is what most
projects need. The individual modules can also be depended upon directly, all under the group
`it.unibo.tuprolog.argumentation`:

| Artifact | Contents |
| --- | --- |
| `arg2p`, `arg2p-jvm`, `arg2p-js` | Aggregate: core engine + causality solver |
| `core`, `core-jvm`, `core-js` | The argumentation engine alone |
| `causality-solver`, `causality-solver-jvm`, `causality-solver-js` | [Causal reasoning]({{% ref "/docs/causality" %}}) |
| `actor-solver` | [Distributed evaluation]({{% ref "/docs/actor-solver" %}}) (JVM only) |
| `ide` | The desktop IDE as a library |

Artifacts without a suffix are the Kotlin Multiplatform ones; use `-jvm` or `-js` to depend on a single
platform explicitly.

## NPM Library

The Arg2P software is available on NPM as a JavaScript library as well. It can be found under the [`@tuprolog` organization](https://www.npmjs.com/org/tuprolog).
To use the library, add the dependency to your `package.json`:

```json
{
  "dependencies": {
    "@tuprolog/arg2p": "{{< version >}}"
  }
}
```

### Usage Example

```js
const arg2p = require('@tuprolog/arg2p').it.unibo.tuprolog.argumentation.bridge.JsBridge

const graph = arg2p.solve('arg2p::solve', `
    f1 :=> d.
    f2 :=> -d.`, `
    graphBuildMode(standard_af).
    statementLabellingMode(statement).
    argumentLabellingMode(grounded).
    orderingPrinciple(last).
    orderingComparator(elitist).
    graphExtension(standardPref).
    queryMode.`, _ => { }).i.next().graph

graph.arguments.forEach(arg => {
    console.log(`${arg.label} : ${arg.descriptor}`)
})
```

For a complete example, see the [repository](https://github.com/tuProlog/arg2p-kt-web).
