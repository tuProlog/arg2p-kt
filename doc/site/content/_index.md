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
- **Arg Flag Tab:** Shows and allows modification of Arg2P flags. Detailed descriptions are on the [Flags Reference]({{% ref "/docs/api/flags" %}}) page.

---

## Arg2p Playground

Try Arg2P directly in your browser using the [Web Playground](https://tuprolog.github.io/arg2p-kt-web/).

{{< resize src="playground.png" size="720x" alt="The playground" >}}

No installation required: the solver runs in the browser. Write an [Arg2P theory]({{% ref "/docs/syntax" %}}), solve a
query one solution at a time, and browse the argument graph of each solution and the stages of its computation. The
[profile]({{% ref "/docs/profiles" %}}) a theory states tells the framework it is read in, and the _Flags_ panel sets
the semantics and every other [flag]({{% ref "/docs/api/flags" %}}), with the value in effect and where it comes from.

An [abstract framework]({{% ref "/docs/modules/abstract" %}}) is a theory too (`graphBuildMode(abstract).`): it is
drawn, and the drawing edits the theory. Arguments and attacks are added, renamed and deleted by drawing, the labelling
of the solution colours them, and hovering or tapping an argument tells its label, what attacks it and what it
attacks. Frameworks are imported from APX, TGF and ICCMA'23 files, and exported to APX and TGF.

{{< resize src="playground-drawing.png" size="720x" alt="An abstract framework, drawn" >}}

Theories are kept in tabs in the browser, shared as links, opened from and saved to files; the playground works
offline once installed as an app, on desktops, tablets and phones.

---

## Arg2p for VS Code

The [extension of VS Code](https://marketplace.visualstudio.com/items?itemName=tuprolog.arg2p) edits theories as
`.arg2p` files: their syntax is highlighted and their syntax errors show as they are typed. **Arg2P: Open the Solver**
opens the solver of the playground beside a theory, running on the JVM: its solutions, their graphs, the drawing of an
abstract framework and the flags. It bundles the language server of Arg2P and the Java runtime it runs on.

{{< resize src="vscode.png" size="720x" alt="Arg2P in VS Code" >}}

---

## Arg2p for AI assistants

The language server of Arg2P is also an [MCP](https://modelcontextprotocol.io/) server, for AI assistants to solve
theories with it: see [Execution]({{% ref "/docs/run#ai-assistants-mcp" %}}).
