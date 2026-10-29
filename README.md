<p align="center"><img src="doc/site/static/logo.svg" width="200" alt="Arg2P logo"></p>

<h1 align="center">Arg2P</h1>

<p align="center">
  <a href="https://github.com/tuProlog/arg2p-kt/actions/workflows/push.yml"><img src="https://github.com/tuProlog/arg2p-kt/actions/workflows/push.yml/badge.svg?branch=master" alt="CI/CD"></a>
  <a href="https://central.sonatype.com/artifact/it.unibo.tuprolog.argumentation/arg2p-jvm"><img src="https://img.shields.io/maven-central/v/it.unibo.tuprolog.argumentation/arg2p-jvm?label=Maven%20Central" alt="Maven Central"></a>
  <a href="https://www.npmjs.com/package/@tuprolog/arg2p"><img src="https://img.shields.io/npm/v/@tuprolog/arg2p?label=npm" alt="npm"></a>
  <a href="https://pypi.org/project/arg2p/"><img src="https://img.shields.io/pypi/v/arg2p?label=PyPI" alt="PyPI"></a>
  <a href="https://tuprolog.github.io/arg2p-kt/coverage/kotlin/index.html"><img src="https://img.shields.io/endpoint?url=https%3A%2F%2Ftuprolog.github.io%2Farg2p-kt%2Fcoverage%2Fkotlin.json" alt="Kotlin coverage"></a>
  <a href="https://tuprolog.github.io/arg2p-kt/coverage/prolog.md"><img src="https://img.shields.io/endpoint?url=https%3A%2F%2Ftuprolog.github.io%2Farg2p-kt%2Fcoverage%2Fprolog.json" alt="Prolog coverage"></a>
  <a href="https://tuprolog.github.io/arg2p-kt/"><img src="https://img.shields.io/badge/docs-wiki-blue" alt="Documentation"></a>
  <a href="LICENSE"><img src="https://img.shields.io/github/license/tuProlog/arg2p-kt" alt="License"></a>
</p>

Arg2P is an implementation of the ASPIC<sup>+</sup> framework for structured argumentation, which reads assumption-based
and abstract frameworks as well. Built on top of the [tuProlog](https://apice.unibo.it/xwiki/bin/view/Tuprolog/) engine,
it runs on the JVM, in JavaScript (Node.js and browsers) and in Python, with the same API; its solver runs in the
browser, in VS Code, and for AI assistants.

The [documentation](https://tuprolog.github.io/arg2p-kt/) starts from
[Getting Started](https://tuprolog.github.io/arg2p-kt/docs/getting-started/).

![The playground](doc/site/assets/playground.png)

---

## Installing

| | |
| --- | --- |
| JVM (Gradle, Maven Central) | `implementation("it.unibo.tuprolog.argumentation:arg2p-jvm:<VERSION>")` |
| JavaScript (npm) | `npm install @tuprolog/arg2p` |
| Python (PyPI) | `pip install arg2p` (the wheel bundles a Java runtime) |
| Browser | the [Web Playground](https://tuprolog.github.io/arg2p-kt-web/): nothing to install |
| VS Code | the [extension](https://marketplace.visualstudio.com/items?itemName=GiuseppePisano.arg2p) |
| Claude Code | `/plugin install arg2p --marketplace tuProlog/arg2p-kt` |
| Codex | `codex plugin marketplace add tuProlog/arg2p-kt`, then install `arg2p` from `/plugins` |

`<VERSION>` is the latest release, shown by the Maven Central badge above. See
[Execution](https://tuprolog.github.io/arg2p-kt/docs/run/) for a first program on each platform, the playground, the
extension of VS Code, and the plugin and MCP server of Arg2P for AI assistants (the other clients, and the server alone).

![Arg2P in VS Code](doc/site/assets/vscode.png)

---

## Issue tracking

If you encounter any issues, please report them on the [GitHub Issues](https://github.com/tuProlog/arg2p-kt/issues) page.  
Your feedback helps improve the project!

---

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
