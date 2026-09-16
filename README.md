# Arg2P

Arg2P is an implementation of the ASPIC<sup>+</sup> framework for structured argumentation.
Built on top of the [tuProlog](https://apice.unibo.it/xwiki/bin/view/Tuprolog/) engine, it supports both JVM and Node environments.

More details are available on the [official wiki](https://tuprolog.github.io/arg2p-kt/).

## References

Arg2P is described in:

- Roberta Calegari, Andrea Omicini, Giuseppe Pisano, Giovanni Sartor.
  **Arg2P: an argumentation framework for explainable intelligent systems.**
  _Journal of Logic and Computation_, 32(2):369–401, 2022.
  [doi:10.1093/logcom/exab089](https://doi.org/10.1093/logcom/exab089)
- Giuseppe Pisano.
  **Argumentation for legal reasoning: meta-models, technology and beyond.**
  PhD thesis, Alma Mater Studiorum — Università di Bologna, 2024.
  [doi:10.48676/unibo/amsdottorato/11671](https://doi.org/10.48676/unibo/amsdottorato/11671)

Individual features — burden of persuasion, defeasible preferences, modularity, causality, distributed
reasoning — have their own papers, listed on the
[References](https://tuprolog.github.io/arg2p-kt/docs/references/) page of the wiki.

### Cite me

If you use Arg2P in academic work, please cite:

```bibtex
@article{arg2p,
  author  = {Calegari, Roberta and Omicini, Andrea and Pisano, Giuseppe and Sartor, Giovanni},
  title   = {{Arg2P}: an argumentation framework for explainable intelligent systems},
  journal = {Journal of Logic and Computation},
  volume  = {32},
  number  = {2},
  pages   = {369--401},
  year    = {2022},
  doi     = {10.1093/logcom/exab089}
}
```

```bibtex
@phdthesis{pisano2024argumentation,
  author  = {Pisano, Giuseppe},
  title   = {Argumentation for legal reasoning: meta-models, technology and beyond},
  school  = {Alma Mater Studiorum --- Universit\`a di Bologna},
  year    = {2024},
  doi     = {10.48676/unibo/amsdottorato/11671}
}
```

---

## Getting Started

Arg2p is available as a [2P-Kt](https://github.com/tuProlog/2p-kt/) library.

#### JVM Library - Gradle

To import the Arg2p module into your Kotlin-based Gradle project, declare the dependency in your `build.gradle(.kts)` file
(the version below is the latest release, see the [releases page](https://github.com/tuProlog/arg2p-kt/releases/latest)):
 ```kotlin
repositories {
    mavenCentral()
}

dependencies {
    implementation("it.unibo.tuprolog.argumentation:arg2p-jvm:0.16.2")
}
 ```

#### Usage Example

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

#### NPM Library

The Arg2P software is available on NPM as a JavaScript library as well. It can be found under the [`@tuprolog` organization](https://www.npmjs.com/org/tuprolog).
To use the library, add the dependency to your `package.json`:

```json
{
  "dependencies": {
    "@tuprolog/arg2p": "0.16.2"
  }
}
```

#### Usage Example

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

---

## Arg2p IDE

The Arg2p IDE is available on the [Releases section of the
GitHub repository](https://github.com/tuProlog/arg2p-kt/releases/latest).

![Java IDE](./imgs/javaide.png)

In the [latest release](https://github.com/tuProlog/arg2p-kt/releases/latest) page, download the _Asset_ named:
```
arg2p-ide-<VERSION>-redist.jar
```
a self-contained, executable Jar containing the 2P-Kt-based Prolog interpreter (`<VERSION>` is the release
version, `0.16.2` at the time of writing).

After you download the jar, you can simply launch it by running:
```bash
java -jar arg2p-ide-0.16.2-redist.jar
```
If your JVM is properly configured, you can also start the IDE by double-clicking the JAR file.

### Features

- **Query Execution:** Write your query in the text field and hit <kbd>Enter</kbd> or click <kbd>&gt;</kbd>.
- **Solution Exploration:** Click <kbd>&gt;</kbd> for the next solution or <kbd>&gt;&gt;</kbd> to compute all solutions.
- **New Query:** Click <kbd>X</kbd> to stop the current query, then enter a new one.

### Additional Tabs

- **Graph Tab:** Displays a graphical representation of the abstract argumentation graph.
- **Arg Flag Tab:** Shows and allows modification of Arg2P flags. Detailed descriptions are on the [Flags Reference](https://tuprolog.github.io/arg2p-kt/docs/flags/) page of the wiki.

---

## Arg2p Playground

Try Arg2P directly in your browser using the [Web Playground](https://tuprolog.github.io/arg2p-kt-web/).

![Web Playground](./imgs/playground.png)

No installation required. It runs in two modes: **Structured**, to write a theory and query it as in the
desktop IDE, and **Abstract**, to draw an argumentation framework and evaluate it with `abstract::solve/5`
under any of the supported semantics. It ships ready-made examples, and setups can be shared as a link.

---

## Issue tracking

If you encounter any issues, please report them on the [GitHub Issues](https://github.com/tuProlog/arg2p-kt/issues) page.  
Your feedback helps improve the project!
