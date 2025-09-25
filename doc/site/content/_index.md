---
title: Home
---

# Arg2P

Arg2P is a lightweight implementation of the ASPIC<sup>+</sup> framework for structured argumentation.
Built on top of the [tuProlog](http://pika-lab.gitlab.io/tuprolog/2p-kt/) engine, it supports both JVM and Node environments.

---

## Arg2p IDE

The Arg2p IDE is available on the [Releases section of the
GitHub repository](https://github.com/tuProlog/arg2p-kt/releases/latest).

{{< resize src="javaide.png" size="560x" alt="Run" >}}

In the [latest release](https://github.com/tuProlog/arg2p-kt/releases/latest) page, download the _Asset_ named:
```
arg2p-ide-ARG2P_VERSION-redist.jar
```
a self-contained, executable Jar containing the 2P-Kt-based Prolog interpreter (`ARG2P_VERSION` will vary depending on the
actual release version).

After you download the `arg2p-ide-ARG2P_VERSION-redist.jar`, you can simply launch it by running:
```bash
java -jar arg2p-ide-ARG2P_VERSION-redist.jar
```
If your JVM is properly configured, you can also start the IDE by double-clicking the JAR file.

### Features

- **Query Execution:** Write your query in the text field and hit <kbd>Enter</kbd> or click <kbd>&gt;</kbd>.
- **Solution Exploration:** Click <kbd>&gt;</kbd> for the next solution or <kbd>&gt;&gt;</kbd> to compute all solutions.
- **New Query:** Click <kbd>X</kbd> to stop the current query, then enter a new one.

### Additional Tabs

- **Graph Tab:** Displays a graphical representation of the abstract argumentation graph.
- **Arg Flag Tab:** Shows and allows modification of Arg2P flags. Detailed descriptions are on the [API page]({{% ref "/docs/predicate" %}}).

---

## Arg2p Playground

Try Arg2P directly in your browser using the [Web Playground](https://tuprolog.github.io/arg2p-kt-web/).

{{< resize src="playground.png" size="560x" alt="Run" >}}

It functions the same as the Java IDE, offering an interactive experience without installation.
