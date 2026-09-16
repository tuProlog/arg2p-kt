---
title: Contributors
weight: 40 
---

## Team
- Giuseppe Pisano (g.pisano@unibo.it)

## Contributors
- Roberta Calegari
- Giovanni Ciatto
- Regis Riveret

## Building from source

The Hugo theme used by this site is a git submodule, so clone the repository recursively:

```bash
git clone --recurse-submodules https://github.com/tuProlog/arg2p-kt.git
```

The build uses the Gradle wrapper, so no local Gradle installation is needed. The most useful tasks are:

| Task | Purpose |
| --- | --- |
| `./gradlew check` | Run every test and check |
| `./gradlew jvmTest` | Run the JVM test suite only |
| `./gradlew jsTest` | Run the JavaScript test suite only |
| `./gradlew ktlintCheck` | Check the code style |
| `./gradlew :ide:run` | Launch the IDE from source |
| `./gradlew :ide:shadowJar` | Build the self-contained IDE jar |
| `./gradlew dokkaGeneratePublicationHtml` | Generate the API documentation |

The JVM tests target Java 21 by default; pass `-PtestJvm=<version>` to use a different one.

### Working on this site

The documentation is a Hugo site under `doc/site`, built through Gradle:

| Task | Purpose |
| --- | --- |
| `./gradlew :doc:hugoServer` | Serve the site locally with live reload |
| `./gradlew :doc:hugoBuild` | Build the static site |

Both download the required Hugo binary automatically. The version shown in the dependency snippets is injected
at build time from the latest git tag, so it never has to be updated by hand.

## Reporting issues

Bugs and feature requests are tracked on [GitHub](https://github.com/tuProlog/arg2p-kt/issues).
