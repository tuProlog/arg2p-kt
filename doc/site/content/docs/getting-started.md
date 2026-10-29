---
title: Getting Started
weight: 5
---

## Getting Started

This page takes you from nothing to a first working evaluation. If you only want to try the framework out,
start with the [Web Playground](#try-it-without-installing-anything) — it needs no project setup at all.

---

## Try it without installing anything

Ready-made environments run Arg2P without any configuration:

- the **[Web Playground](https://tuprolog.github.io/arg2p-kt-web/)**, which runs entirely in your browser;
- the **[extension of VS Code](https://marketplace.visualstudio.com/items?itemName=GiuseppePisano.arg2p)**, which edits
  theories as `.arg2p` files and solves them beside the editor.

Both accept a theory, run queries against it, draw the resulting argumentation graph and let you change the
[flags]({{% ref "/docs/api/flags" %}}) interactively.

---

## Your first theory

Arg2P reasons about theories made of rules that can conflict with each other. Here are two premises that
support opposite conclusions:

```prolog
f1 :=> d.
f2 :=> -d.
```

`:=>` introduces a *defeasible premise*, and `-` is strong negation. So this theory says: there is a reason to
believe `d`, and a reason to believe the opposite. Neither wins, and argumentation tells us precisely that.

Ask the engine what it makes of `d`:

```prolog
?- arg2p::solve(d, Res).

Res = [und(d)]
```

`d` is **undecided**: it is supported by an argument, that argument is attacked by the one for `-d`, and
neither defeats the other. Add a preference between the two premises:

```prolog
f1 :=> d.
f2 :=> -d.
sup(f1, f2).
```

and the answer changes:

```prolog
?- arg2p::solve(d, Res).

Res = [in(d)]
```

`f1` is now stronger, so its argument defeats the other and `d` is accepted.

Both versions can be run without installing anything:
[the first one](https://tuprolog.github.io/arg2p-kt-web/?mode=structured&theory=f1%20%3A%3D%3E%20d.%0Af2%20%3A%3D%3E%20-d.&query=arg2p%3A%3Asolve%28d%2C%20Res%29)
and [the one with the preference](https://tuprolog.github.io/arg2p-kt-web/?mode=structured&theory=f1%20%3A%3D%3E%20d.%0Af2%20%3A%3D%3E%20-d.%0Asup%28f1%2C%20f2%29.&query=arg2p%3A%3Asolve%28d%2C%20Res%29).

The [Language]({{% ref "/docs/syntax" %}}) page describes the full syntax: strict rules, undercutting,
obligations and permissions, burden of persuasion.

---

## Using it from Kotlin

Add the dependency:

```kotlin
repositories {
    mavenCentral()
}

dependencies {
    implementation("it.unibo.tuprolog.argumentation:arg2p-jvm:{{< version >}}")
}
```

Then evaluate a theory and read the labelling:

```kotlin
import it.unibo.tuprolog.argumentation.Arg2p

fun main() {
    val (graph) =
        Arg2p.of(
            """
            f1 :=> d.
            f2 :=> -d.
            """.trimIndent(),
        ).graphs()

    graph.labellings.forEach {
        println("${it.label} : ${it.argument.conclusion}")
    }
}
```

which prints:

```text
und : -d
und : d
```

Both conclusions are undecided.

The [Kotlin API]({{% ref "/docs/api/kotlin" %}}) page covers the flags, the result model, Prolog goals, causality and
how to embed Arg2P in an existing 2P-Kt solver; the same API is in [JavaScript]({{% ref "/docs/api/javascript" %}}) and
[Python]({{% ref "/docs/api/python" %}}).

---

## Using it from JavaScript

```json
{
  "dependencies": {
    "@tuprolog/arg2p": "{{< version >}}"
  }
}
```

```js
import { Arg2p } from '@tuprolog/arg2p'

const [graph] = Arg2p.of(`
    f1 :=> d.
    f2 :=> -d.`).graphs()

graph.labellings.forEach(it => console.log(`${it.label} : ${it.argument.descriptor}`))
```

See [JavaScript API]({{% ref "/docs/api/javascript" %}}) for the whole API.

---

## Where to go next

| If you want to… | Read |
| --- | --- |
| Learn the theory language | [Language]({{% ref "/docs/syntax" %}}) |
| Know which queries you can ask | [API & Flags]({{% ref "/docs/api/engine" %}}) |
| Change semantics or preferences | [Flags Reference]({{% ref "/docs/api/flags" %}}) |
| Evaluate a plain abstract framework | [Abstract Evaluation]({{% ref "/docs/modules/abstract" %}}) |
| See complete, runnable theories | [Examples]({{% ref "/docs/examples" %}}) |
| Understand the engine's structure | [Modules]({{% ref "/docs/modules" %}}) |
| Fix something that went wrong | [Troubleshooting]({{% ref "/docs/troubleshooting" %}}) |
