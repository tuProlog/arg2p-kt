# arg2p for Python

[Arg2P](https://github.com/tuProlog/arg2p-kt), structured argumentation in Prolog (ASPIC+, ABA, ABA+, abstract
frameworks, ...), from Python: the API of the library of the JVM, in snake case, run through
[JPype](https://jpype.readthedocs.io) on the Java runtime the wheel bundles.

```sh
pip install arg2p
```

```python
from arg2p import Arg2p, Flags, Semantics

arg2p = Arg2p.of("""
r1 : [] => a.
r2 : [] => -a.
sup(r1, r2).
""")

arg2p.answer("a").in_statements                 # ['a']
(graph,) = arg2p.graphs()
graph.conclusions("out")                        # ['-a']

preferred = Arg2p.of("f1 :=> d.\nf2 :=> -d.", Flags.defaults().argument_labelling_mode(Semantics.PREFERRED))
len(preferred.graphs())                         # 2

Arg2p.of("r1 :=> a.\nr2 : a => b.").causality.but_for("a", "b")   # True
```

See [the documentation](https://tuprolog.github.io/arg2p-kt/docs/api/python/) for the whole API: flags, graphs,
Prolog goals, causality, the parallel solver and the command line (`arg2p theory.pl -q a`).

## The MCP server

`arg2p mcp` runs the MCP server of Arg2P, for AI assistants to solve theories with it (the tools `solve`, `answer`,
`check`, `settings` and `explore`), on the Java runtime of the wheel. With [uv](https://docs.astral.sh/uv/), nothing
to install first:

```sh
uvx arg2p mcp
```

See [AI assistants](https://tuprolog.github.io/arg2p-kt/docs/run/#ai-assistants-mcp) for the clients, and the plugin
with the skills of Arg2P.

<!-- mcp-name: io.github.tuProlog/arg2p -->

## Building

The package is a project of the Gradle build of Arg2P, which gives it its jars, its version and its Java runtime:

```sh
./gradlew :python:pythonJars :python:pythonVersion :python:pythonRuntime
pip install "./python[test]"
pytest python/tests
```

Without the runtime (`pythonRuntime`), the package runs on the Java of the system (21 or later). `ARG2P_JVM` names
another library of the JVM, `ARG2P_CLASSPATH` other jars, `ARG2P_STACK` the stack of its threads (`512m`) and
`ARG2P_JVM_OPTIONS` its other options (as `-Xmx8g`, for the heap of large graphs).
