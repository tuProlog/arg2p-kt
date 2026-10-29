# Arg2P for VS Code

Structured argumentation in Prolog with [Arg2P](https://tuprolog.github.io/arg2p-kt/): write theories (ASPIC<sup>+</sup>,
ABA, abstract frameworks), see where they do not parse as you type, solve queries and browse their argument graphs,
in the editor.

![A theory and its solver](https://raw.githubusercontent.com/tuProlog/arg2p-kt/master/imgs/vscode.png)

## Theories

A theory is a `.arg2p` file: its syntax is highlighted, and its syntax errors show as you type, as problems of the
document. Its profile (`graphBuildMode(aba).`, say) tells the framework it is read in: see
[Profiles](https://tuprolog.github.io/arg2p-kt/docs/profiles/).

```prolog
% Tweety, the bird that does not fly
f1 :=> bird(tweety).
r1 : bird(X) => flies(X).
f2 :=> -flies(tweety).
```

## The solver

**Arg2P: Open the Solver** (the ▷ of the editor of a theory) opens its solver beside it (or under it: `arg2p.solver.position`):

- the query, solved with **Run** (or Enter), one solution at a time (**Next**), **Stop** to stop it (the query alone:
  the other solvers and the language server go on);
- the argument graph of a solution, the stages of its computation, and the popup of each argument;
- for an abstract framework (`graphBuildMode(abstract).`), its drawing instead: what is drawn is written in the theory,
  the theory edited is drawn again, and each argument tells its label, what attacks it and what it attacks;
- the flags, the profile first, each with the value in effect and where it comes from;
- what the theory prints.

Solving a theory that does not parse takes to where it does not, in its editor. The solver keeps the query and the flags of each theory.

## The language server

The extension runs the language server of Arg2P on the JVM: on the Java the settings name (`arg2p.java.path`), or else
on the Java runtime the package for a platform bundles, or else on that of `JAVA_HOME`, or else on `java` on the path
(21 or later). `arg2p.java.options` are the options of the JVM: `-Xmx8g`, say, for the heap of large graphs. Both are
user settings alone: a workspace cannot set them. **Arg2P: Restart the Language Server** starts it afresh.

## AI assistants

The language server is also the MCP server of Arg2P, which the extension registers for the assistants of VS Code
(Copilot agent mode, VS Code 1.101 or later): they solve theories with its tools rather than guessing what they
conclude.

| Tool | |
| --- | --- |
| `solve` | The solutions of a query on a theory, with their bindings and the argument graph |
| `answer` | The statements of a query labelled in, out and und |
| `check` | Whether a theory or a query parses, and where it does not |
| `settings` | The profile and the flags in effect for a theory |
| `explore` | Shows the solver of a theory in the chat, to explore its results there |

It runs on the Java of the language server. For other assistants, see
[Execution](https://tuprolog.github.io/arg2p-kt/docs/run/#ai-assistants-mcp).

## Settings

| Setting | |
| --- | --- |
| `arg2p.java.path` | The `java` running the language server (user settings) |
| `arg2p.java.options` | The options of the JVM running it (`-Xss512m`, user settings) |
| `arg2p.flags` | The flags a theory is first solved with |
| `arg2p.solver.position` | Where the solver opens: beside the theory (`beside`) or under it (`below`) |
