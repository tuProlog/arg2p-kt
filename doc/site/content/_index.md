---
title: Home
---

# Arg2P

Arg2P is a lightweight implementation of the ASPIC<sup>+</sup> framework for structured argumentation.
Built on top of the [tuProlog](https://apice.unibo.it/xwiki/bin/view/Tuprolog/) engine, it supports both JVM and Node environments.

New here? The [Getting Started]({{% ref "/docs/getting-started" %}}) page takes you from nothing to a first
working evaluation.

---

## Arg2p IDE

The Arg2p IDE is available on the [Releases section of the
GitHub repository](https://github.com/tuProlog/arg2p-kt/releases/latest).

{{< resize src="javaide.png" size="560x" alt="Run" >}}

In the [latest release](https://github.com/tuProlog/arg2p-kt/releases/latest) page, download the _Asset_ named:
```
arg2p-ide-{{< version >}}-redist.jar
```
a self-contained, executable Jar containing the 2P-Kt-based Prolog interpreter (the version shown here is the latest
release, {{< version >}}).

After you download the `arg2p-ide-{{< version >}}-redist.jar`, you can simply launch it by running:
```bash
java -jar arg2p-ide-{{< version >}}-redist.jar
```
If your JVM is properly configured, you can also start the IDE by double-clicking the JAR file.

### Features

- **Query Execution:** Write your query in the text field and hit <kbd>Enter</kbd> or click <kbd>&gt;</kbd>.
- **Solution Exploration:** Click <kbd>&gt;</kbd> for the next solution or <kbd>&gt;&gt;</kbd> to compute all solutions.
- **New Query:** Click <kbd>X</kbd> to stop the current query, then enter a new one.

### Additional Tabs

- **Graph Tab:** Displays a graphical representation of the abstract argumentation graph.
- **Arg Flag Tab:** Shows and allows modification of Arg2P flags. Detailed descriptions are on the [Flags Reference]({{% ref "/docs/flags" %}}) page.

---

## Arg2p Playground

Try Arg2P directly in your browser using the [Web Playground](https://tuprolog.github.io/arg2p-kt-web/).

{{< resize src="playground.png" size="560x" alt="Run" >}}

No installation required. The playground runs in either of two modes, selected from the toolbar:

- **Structured** — write an [Arg2P theory]({{% ref "/docs/syntax" %}}) and run any query against it, as in the
  desktop IDE.
- **Abstract** — draw an argumentation framework instead of writing rules. The query is generated for you as
  [`abstract::solve/5`]({{% ref "/docs/modules/abstract" %}}), and the drawing is recoloured with the labelling of the
  solution being shown. A _Semantics_ selector next to the query sets the
  [`argumentLabellingMode`]({{% ref "/docs/flags" %}}) flag, covering every semantics the engine implements.

It also ships a handful of ready-made examples, and _Share_ builds a link that restores whatever is currently
loaded — handy for sending a case to someone else.
