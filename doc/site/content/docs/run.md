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

For a complete example, check out the [GitHub demo](https://github.com/Gilbocc/arg2p-kt-demo).

### Available modules

`arg2p-jvm` is the aggregate module: it brings in the API of Arg2P (`Arg2p`), with the core engine, the causality and
the parallel solver, and is what most projects need. The individual modules can also be depended upon directly, all under the group
`it.unibo.tuprolog.argumentation`:

| Artifact | Contents |
| --- | --- |
| `arg2p`, `arg2p-jvm`, `arg2p-js` | Aggregate: the API of Arg2P, with the core engine, the causality and the parallel solver |
| `core`, `core-jvm`, `core-js` | The argumentation engine alone |
| `causality-solver`, `causality-solver-jvm`, `causality-solver-js` | [Causal reasoning]({{% ref "/docs/modules/causality" %}}) |
| `parallel-solver`, `parallel-solver-jvm`, `parallel-solver-js` | [Parallel graph construction]({{% ref "/docs/modules/parallel-solver" %}}) |
| `actor-solver` | [Distributed evaluation]({{% ref "/docs/modules/actor-solver" %}}) (JVM only) |

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
import { Arg2p } from '@tuprolog/arg2p'

const [graph] = Arg2p.of(`
    f1 :=> d.
    f2 :=> -d.`).graphs()

graph.labellings.forEach(it => console.log(`${it.label} : ${it.argument.descriptor}`))
```

For a complete example, see the [repository](https://github.com/tuProlog/arg2p-kt-web).

## Python

The package [`arg2p`](https://pypi.org/project/arg2p/) runs the same API from Python: see [Python]({{% ref "/docs/api/python" %}}).

## Web Playground

The [playground](https://tuprolog.github.io/arg2p-kt-web/) runs Arg2P in the browser, with nothing to install: write a
theory, solve a query, and browse its argument graph and the stages of its computation; draw an abstract framework
(the drawing is its theory), and set the profile and the flags. It keeps your theories in tabs, shares them as links,
and works offline once installed as an app. Its components are on npm as
[`@tuprolog/arg2p-ui`](https://www.npmjs.com/package/@tuprolog/arg2p-ui), to embed the solver in other pages.

## VS Code

The [extension of VS Code](https://marketplace.visualstudio.com/items?itemName=GiuseppePisano.arg2p) edits theories as
`.arg2p` files: their syntax errors show as they are typed, and **Arg2P: Open the Solver** opens the solver of the
playground beside a theory, on the JVM. It bundles the language server of Arg2P and the Java runtime it runs on.

### Federation and parallel solving

The switch **Parallel** of the solver solves the query on the
[parallel solver]({{% ref "/docs/modules/parallel-solver#federation" %}}): any theory, flags and query (query mode
too), on the workers of the language server, and on those of the devices linked to it. With none linked, it solves on
the workers of this device alone, concurrently. The solver keeps the switch of each theory with its query and flags; the
graph of a solution is that of the last stage of its computation, the stages before it are not kept.

The language servers of the same local network see each other: the view **Arg2P Federation** (in the explorer, or
**Arg2P: Show the Federation**) shows the name of this device and its 4-digit code, and the devices nearby, each linked
to once its code is typed (shown on that device); linked once, two devices link again by themselves while they run.
It lists the devices linked with their workers, and **New code** refuses the code shown from then on.

A phone joins too: the view shows the QR code of a page the language server serves on the local network
(`http://<address>:<port>/`). Scanned with a phone on the same network, the page asks for the code of the device, and
the phone lends the workers of its Web Workers while the page stays open. The page loads `@tuprolog/arg2p` of the same
version as the extension from [jsDelivr](https://www.jsdelivr.com/package/npm/@tuprolog/arg2p).

| Setting | |
| --- | --- |
| `arg2p.federation.enabled` | Whether the language server is seen by the devices nearby and lets phones join (`true` by default): disabled, it neither listens nor is heard, and **Parallel** solves on this device alone |
| `arg2p.federation.name` | The name of this device, as the others see it (the name of the host by default) |

The language server speaks of its federation with the requests `arg2p/federation` (its state), `arg2p/federation/pair`
(`{ site, code }`), `arg2p/federation/newCode`, `arg2p/federation/rename` (`{ name }`) and `arg2p/federation/enable`
(`{ enabled }`), each answered with the state, and the notification `arg2p/federationChanged`; the editor enables it
with the options of `initialize` (`{ federation: { enabled, name } }`), and a query of the request `arg2p/engine` is
solved on it with `parallel: true`.

## AI assistants (MCP)

The language server of Arg2P is also an [MCP](https://modelcontextprotocol.io/) server: AI assistants solve theories
with it rather than guessing what they conclude. Its tools:

| Tool | Takes | Gives |
| --- | --- | --- |
| `solve` | `theory`, `query`, `flags`, `maxSolutions`, `timeoutSeconds` | The solutions of a query (default `buildLabelSets(In, Out, Und)`) on a theory, with their bindings and the argument graph of the last one: every argument with its conclusion, top rule and label, every attack with its type |
| `answer` | `theory`, `query`, `flags`, `timeoutSeconds` | The statements matching a literal (`flies(X)`, `-guilty`) labelled in, out and und |
| `check` | `text`, `kind` (`theory`, the default, or `query`) | Whether the text parses, and where it does not (line, column, token) |
| `settings` | `theory`, `flags`, `timeoutSeconds` | The value in effect of every flag for a theory: its profile, its semantics, ... |
| `explore` | `theory`, `query`, `flags`, `timeoutSeconds` | Shows the user the solver of a theory, to explore its results themselves |

The `theory` is text, the `flags` Prolog facts; `theory`, and the `query` of `answer`, are required (and the `text` of
`check`). A theory that does not parse, or that its profile refuses, is an error of the tool saying why; a query is
stopped after `timeoutSeconds` (60 by default).

`explore` is an [MCP App](https://modelcontextprotocol.io/docs/extensions/apps): in the clients that show them
(Claude, ChatGPT, VS Code), the solver of the playground appears in the conversation, runs the query, and lets the user
browse its solutions, their argument graphs and stages, the drawing of an abstract framework and the flags, and solve
again. Everywhere, its result links the [web playground](https://tuprolog.github.io/arg2p-kt-web/) with the same
theory, query and flags.

Where the MCP server does not run but the assistant runs code (claude.ai with code execution), the skills install the
Python package and make the page of a run with `arg2p page` (see [Python]({{% ref "/docs/api/python#the-recorded-page" %}})):
an HTML file showing the solutions, their graphs, the flags in effect and the theory, which claude.ai shows in the
conversation. It shows the run as it was solved: another query is solved again, in a new page.

### The plugin and its skills

The plugin `arg2p` bundles the MCP server with **skills**, the know-how of Arg2P for an assistant: `arg2p-formalize`
(turning a problem into a theory, its profile and its flags), `arg2p-solve` (solving it, checking and reporting the
result) and `arg2p-explore` (showing the user the solver). Its MCP server runs from the Python package with
[uv](https://docs.astral.sh/uv/) (`uvx arg2p mcp`).

| Client | |
| --- | --- |
| Claude Code | `/plugin install arg2p --marketplace tuProlog/arg2p-kt` (or `/plugin marketplace add tuProlog/arg2p-kt`, then `/plugin install arg2p@arg2p`) |
| Claude (claude.ai, Desktop) | upload `arg2p-plugin-{{< version >}}.zip` of the [latest release](https://github.com/tuProlog/arg2p-kt/releases/latest) in _Customize > Plugins_, or a skill alone (`arg2p-formalize-skill-{{< version >}}.zip`, ...) in _Customize > Skills_; in chats local MCP servers do not run, so add the server to Claude Desktop (below) for the skills to solve |
| Codex, ChatGPT | `codex plugin marketplace add tuProlog/arg2p-kt`, then install `arg2p` from `/plugins` |
| Any agent reading [Agent Skills](https://agentskills.io) | `npx skills add tuProlog/arg2p-kt` |

### The MCP server alone

The extension of VS Code registers it for the assistants of VS Code (Copilot agent mode): nothing to configure.
Elsewhere, with [uv](https://docs.astral.sh/uv/) (the Python package bundles its Java runtime), in Claude Code:

```bash
claude mcp add arg2p -- uvx arg2p mcp
```

In clients configured by JSON (Claude Desktop, Cursor, ...):

```json
{
  "mcpServers": {
    "arg2p": { "command": "uvx", "args": ["arg2p", "mcp"] }
  }
}
```

It is listed in the [MCP Registry](https://registry.modelcontextprotocol.io) as `io.github.tuProlog/arg2p`. With Java
{{< java >}} or later instead, download `arg2p-lsp-{{< version >}}-redist.jar` from the
[latest release](https://github.com/tuProlog/arg2p-kt/releases/latest) and run `java -jar
arg2p-lsp-{{< version >}}-redist.jar --mcp`.

The server speaks MCP over its standard input and output (protocol versions 2025-06-18, 2025-03-26 and 2024-11-05).
Without `--mcp`, the same jar is the language server of Arg2P (the Language Server Protocol, over the same streams),
for editors other than VS Code.
