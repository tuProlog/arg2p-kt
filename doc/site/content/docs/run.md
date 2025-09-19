---
title: Run
weight: 10 
---

## Standalone Application

Follow the __[GETTING STARTED]({{% ref "/" %}})__ instructions to install and run the Arg2p Java IDE.

The Arg2P IDE should appear as depicted below:

{{< resize src="run1.png" size="500x" alt="Run" >}}

Copy the following example theory in the IDE editor (or create your own following the [base instructions]({{% ref "/docs/syntax" %}})):

```prolog
d1 : bird(X) => flies(X).
d2 : penguin(X) => bird(X).
s1 : penguin(X) -> -flies(X).
a1 :-> penguin(tweety).
```

Run the goal `buildLabelSets` to build and evaluate the entire framework:

{{< resize src="run3.png" size="460x" alt="Run" >}}

Labellings are printed textually in the _Output_ tab and graphically in the _Graph_ tab:

{{< resize src="run4.png" size="460x" alt="Run" >}}

Alternatively you can require the evaluation of a single statement with the `answerQuery/4` predicate. For example, run the goal
`answerQuery(flies(tweety), In, Out, Und)` to check the admissibility of the `flies(tweety)` statement. The result is printed in the _Solution_ tab:

{{< resize src="run5.png" size="460x" alt="Run" >}}

More information on the Arg2p usage can be found on the [API & Flags page]({{% ref "/docs/predicate" %}}).

#### Kotlin library

Follow the __[Get Started]({{% ref "/" %}})__ instructions to add the Arg2p dependency to your Kotlin project.

First include the library with:
  
```kotlin
import it.unibo.tuprolog.argumentation.core.Arg2pSolver
```

You can create a new Prolog solver including the Arg2p library with the following notation (for more info on 2P-Kt refer to the [official page](https://gitlab.com/pika-lab/tuprolog/2p-in-kotlin)):

```kotlin
val arg2p = Arg2pSolver.default()
val solver = ClassicSolverFactory.mutableSolverWithDefaultBuiltins(
    otherLibraries = arg2p.to2pLibraries()
)
```

Set the solver theory:

```kotlin
solver.loadStaticKb(Theory.parse("""
            graphBuildMode(standard_af).
            argumentLabellingMode(grounded).
            statementLabellingMode(statement).
            
            d1 : bird(X) => flies(X).
            d2 : penguin(X) => bird(X).
            s1 : penguin(X) -> -flies(X).
            a1 :-> penguin(tweety).

        """.trimIndent(), arg2p.operators()))
```

Require the evaluation of the theory. For example:

```kotlin
prolog {
    solver.solve("buildLabelSets"(X, Y, Z))
        .map { 
            when(it) {
                Solution.Yes -> "In statements: ${it.substitution[X].toString()}"
                Solution.No -> "No available solution"
                Solution.Halt -> "Error in resolution process"
            } 
        }
}
```





