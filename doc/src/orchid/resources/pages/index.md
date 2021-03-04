---
title: Home
---

# Arg2P

Quick links:

- [GitLab Repository](https://gitlab.com/pika-lab/argumentation/arg2p-kt)
- [GitHub Repository](https://github.com/tuProlog/arg2p-kt)
- [Maven Repository](https://search.maven.org/search?q=g:it.unibo.tuprolog.argumentation)
- [Bintray Repository](https://bintray.com/pika-lab/argumentation)
- [NPM Package](https://www.npmjs.com/package/@tuprolog/arg2p-core)
- [Documentation](https://pika-lab.gitlab.io/argumentation/arg2p-kt)


Arg2Prolog is a lightweight implementation of the ASPIC<sup>+</sup>-like framework for structured argumentation.
It is built on the top of the [tuProlog](http://pika-lab.gitlab.io/tuprolog/2p-in-kotlin/) engine.

## Get Started

The Arg2p library is available in two different forms: as a standalone application (Arg2p Java IDE), and as a [2P-Kt](http://pika-lab.gitlab.io/tuprolog/2p-in-kotlin/) library.

### Graphical User Interface

If you need a GUI for your Prolog interpreter, you can rely on the Arg2p IDE which is available on the [Releases section of the
GitHub repository](https://github.com/tuProlog/arg2p-kt/releases).

The page of the [latest release](https://github.com/tuProlog/arg2p-kt/releases/latest) of Arg2P exposes a number of _Assets_.
There, the one named:
```
arg2p-ide-VERSION-redist.jar
```
is the self-contained, executable Jar containing the 2P-Kt-based Prolog interpreter (`VERSION` may vary depending on the
actual release version).

After you download the `arg2p-ide-VERSION-redist.jar`, you can simply launch it by running:
```bash
java -jar arg2p-ide-VERSION-redist.jar
```
However, if you have properly configured the JVM on your system, it may be sufficient to just double-click on the JAR to start the IDE.

There, one may query the Arg2p Prolog interpreter against the currently opened theory file, which can of course be
loaded from the user's file system by pressing <kbd>File</kbd> and then <kbd>Open...</kbd>.

To issue a query, the user must write it in the query text field, at the center of the application.
By either pressing <kbd>Enter</kbd> while the cursor is on the query text field, or by clicking on the <kbd>&gt;</kbd> button, the user can start a new resolution process, aimed at solving the provided query.
Further solutions can be explored by clicking on the <kbd>&gt;</kbd> over and over again.
One may also compute all the unexplored solutions at once by clicking on the <kbd>&gt;&gt;</kbd> button.

To perform a novel query, the user may either:
- write the new query in the query text field, and then press <kbd>Enter</kbd>, or
- click on the <kbd>R</kbd> (Reset) button, write the new query in the query text field, and then press <kbd>&gt;</kbd>.

For a deeper introduction on the Arg2p IDE you can refer to the original [2P-Kt](https://gitlab.com/pika-lab/tuprolog/2p-in-kotlin) page.
The Arg2p version of the IDE introduces two additional tabs:
- the _Graph_ tab, containing the graphical representation of the abstract argumentation graph (available only if leveraging the abstract query mode -- `buildLabelSet` predicate);
- the _Arg Flag_ tab, allowing the user to see/modify the values of the Arg2p flags. A comprehensive description of these values can be found on the [official wiki](https://pika-lab.gitlab.io/argumentation/arg2p-kt/).

### Gradle users

To import the Arg2p module (version `ARG2P_VERSION`) into your Kotlin-based project leveraging on Gradle,
you simply need to declare the corresponding dependency in your `build.gradle(.kts)` file:
 ```kotlin
dependencies {
    implementation("it.unibo.tuprolog.argumentation", "core", "ARG2P_VERSION")
}
 ``` 

Don't forget to tell Gradle to use Maven Central as a source for dependency lookup. You can do it as follows:
```kotlin
repositories {
    mavenCentral()
}
``` 

#### JVM-only projects with Gradle

In the case your project only targets the JVM platform, remember to add the `-jvm` suffix to the module name:
 ```kotlin
dependencies {
    implementation("it.unibo.tuprolog.argumentation", "core-jvm", "ARG2P_VERSION")
}
 ``` 

### Maven users

To import the Arg2p module (version `ARG2P_VERSION`) into your Kotlin-based project leveraging on Maven,
you simply need to declare the corresponding dependency in your `pom.xml` file:
 ```xml
<dependency>
    <groupId>it.unibo.tuprolog.argumentation</groupId>
    <artifactId>core</artifactId>
    <version>ARG2P_VERSION</version>
</dependency>
 ``` 

#### JVM-only projects with Maven

In the case your project only targets the JVM platform, remember to add the `-jvm` suffix to the module name:
 ```xml
<dependency>
    <groupId>it.unibo.tuprolog.argumentation</groupId>
    <artifactId>core-jvm</artifactId>
    <version>ARG2P_VERSION</version>
</dependency>
 ``` 

### NPM users

The Arg2P software is available on NPM as a JavaScript library as well. It can be found under the [`@tuprolog` organization](https://www.npmjs.com/org/tuprolog).
To import the Arg2p library into your `package.json`, it is sufficient to declare your dependency as follows:
```json
{
  "dependencies": {
    "@tuprolog/arg2p-core": "^ARG2P_MODULE_VERSION"
  }
}
```

## Issue tracking

If you meet some problem in using Arg2p, you are encouraged to signal it through the project ["Issues" section](https://gitlab.com/pika-lab/argumentation/arg2p-kt/-/issues) on GitLab.
