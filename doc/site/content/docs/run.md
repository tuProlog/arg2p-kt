---
title: Execution
weight: 10 
---

## JVM Library - Gradle

Arg2p is available as a [2P-Kt](http://pika-lab.gitlab.io/tuprolog/2p-kt/) library.

To import the Arg2p module (version `ARG2P_VERSION`) into your Kotlin-based Gradle project, declare the dependency in your `build.gradle(.kts)` file:
 ```kotlin
repositories {
    mavenCentral()
}

dependencies {
    implementation("it.unibo.tuprolog.argumentation:arg2p-jvm:ARG2P_VERSION")
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

## NPM Library

The Arg2P software is available on NPM as a JavaScript library as well. It can be found under the [`@tuprolog` organization](https://www.npmjs.com/org/tuprolog).
To use the library, add the dependency to your `package.json`:

```json
{
  "dependencies": {
    "@tuprolog/arg2p": "ARG2P_VERSION"
  }
}
```

### Usage Example

```js
const arg2p = require('@tuprolog/arg2p').it.unibo.tuprolog.argumentation.bridge.JsBridge

const graph = arg2p.solve('buildLabelSets', `
    f1 :=> d.
    f2 :=> -d.`, `
    graphBuildMode(standard_af).
    statementLabellingMode(statement).
    argumentLabellingMode(grounded_hash).
    orderingPrinciple(last).
    orderingComparator(elitist).
    graphExtension(standardPref).
    queryMode.`, _ => { }).i.next().graph

graph.arguments.forEach(arg => {
    console.log(`${arg.label} : ${arg.descriptor}`)
})
```

For a complete example, see the [repository](https://github.com/tuProlog/arg2p-kt-web).
