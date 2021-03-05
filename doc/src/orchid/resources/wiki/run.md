---
---

### Run

#### Standalone Application

- Follow the __[Get Started]({{ site.baseUrl }})__ instructions to install and run the Arg2p Java IDE.
- The Arg2P IDE should appear as depicted below:

<p align="center">
  <img width="460" src={{ 'assets/media/run1.png'|asset|scale(0.85)  }}>
</p>

- Copy the following example theory in the IDE editor (or create your own following the [base instructions]({{ site.baseUrl }}/wiki/syntax)):

```prolog
d1 : bird(X) => flies(X).
d2 : penguin(X) => bird(X).
s1 : penguin(X) -> -flies(X).
a1 :-> penguin(tweety).
```

- Run the goal `buildLabelSets` to build and evaluate the entire framework:

<p align="center">
  <img width="460" src={{ 'assets/media/run3.png'|asset|scale(0.85)  }}>
</p>

- Labellings are printed textually in the _Output_ tab and graphically in the _Graph_ tab:

<p align="center">
  <img width="460" src={{ 'assets/media/run4.png'|asset|scale(0.85)  }}>
</p>

- Alternatively you can require the evaluation of a single statement with the `answerQuery/4` predicate. For example, run the goal
`answerQuery(flies(tweety), In, Out, Und)` to check the admissibility of the `flies(tweety)` statement. The result is printed in the _Solution_ tab:

<p align="center">
  <img width="460" src={{ 'assets/media/run5.png'|asset|scale(0.85)  }}>
</p>

More information on the Arg2p usage can be found on the [API & Flags page]({{ site.baseUrl }}/wiki/predicate).

#### Kotlin library

- Follow the __[Get Started]({{ site.baseUrl }})__ instructions to add the Arg2p dependency to your Kotlin project.

- First include the library with:
  
```kotlin
import it.unibo.tuprolog.argumentation.Arg2pLibrary
```

- You can create a new Prolog solver including the Arg2p library with the following notation (for more info on 2P-Kt refer to the [official page](https://gitlab.com/pika-lab/tuprolog/2p-in-kotlin)):

```kotlin
val solver = ClassicSolverFactory.mutableSolverWithDefaultBuiltins(
    otherLibraries = Libraries.of(Arg2pLibrary.get())
)
```

- Set the solver theory:

```kotlin
solver.loadStaticKb(Theory.parse("""
            graphBuildMode(base).
            argumentLabellingMode(grounded).
            statementLabellingMode(base).
            orderingPrinciple(last).
            orderingComparator(elitist).
            d1 : bird(X) => flies(X).
            d2 : penguin(X) => bird(X).
            s1 : penguin(X) -> -flies(X).
            a1 :-> penguin(tweety).
        """.trimIndent()))
```

- Require the evaluation of a goal. For example:

```kotlin
prolog {
    solver.solve("buildLabelSets"(In, Out, Und))
        .map { 
            when(it) {
                Solution.Yes -> "In statements: ${it.substitution[In].toString()}"
                Solution.No -> "No available solution"
                Solution.Halt -> "Error in resolution process"
            } 
        }
}
```





