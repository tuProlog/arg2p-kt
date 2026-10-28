---
title: Causality
weight: 27
---

## Causality

The causality solver answers questions of the form *"was this the cause of that?"* over an argumentation
theory. It is a separate module, built on top of the core engine, and it adds a small set of Prolog predicates
implementing counterfactual and NESS causal tests.

It is available as the Maven module `causality-solver` and as the npm package
`@tuprolog/arg2p-causality-solver`. It is also bundled in the
[IDE]({{% ref "/docs/getting-started" %}}), ready to use with no extra setup, and in the
[API of Arg2P]({{% ref "/docs/api/kotlin" %}}) (`arg2p`, the npm package `@tuprolog/arg2p`, the Python package
`arg2p`): every solver of the API has it.

---

## From the API

Every solver of the [API of Arg2P]({{% ref "/docs/api/kotlin" %}}) tests causality on its theory, through its
`causality`, one method for each predicate below, the statements given as Prolog text:

```kotlin
import it.unibo.tuprolog.argumentation.Arg2p

val causality = Arg2p.of("r_0 : exposure => disease.\nf_1 :=> exposure.").causality
causality.ness("exposure", "disease")      // the intervention, [-exposure]: null if it is not a NESS cause
causality.butFor("exposure", "disease")    // true
```

| Method | Predicate |
| --- | --- |
| `butFor(cause, effect)` | `but_for/2` |
| `ness(cause, effect)`, `nessOriginal(cause, effect)` | `ness/3`, `ness_original/3`: the intervention, or null |
| `nessIntervention(intervention, effect)`, `nessOriginalIntervention(intervention, effect)` | `ness_intervention/2`, `ness_original_intervention/2` |

The same methods are in [JavaScript]({{% ref "/docs/api/javascript" %}}) and, in snake case (`but_for`,
`ness_original`, ...), in [Python]({{% ref "/docs/api/python" %}}). Each test starts from a reset context, as the
others before it left nothing behind.

---

## Getting a causality solver of 2P-Kt

From Kotlin, a solver of 2P-Kt with the causality module is the `causality` extension on `Arg2pSolverFactory`:

```kotlin
import it.unibo.tuprolog.argumentation.causality.causality
import it.unibo.tuprolog.argumentation.core.Arg2pSolverFactory

val solver = Arg2pSolverFactory.causality(
    """
    r_0 : exposure => disease.
    f_1 :=> exposure.
    """.trimIndent(),
)
```

It returns a ready-to-use 2P-Kt `MutableSolver` with the causality module loaded.

---

## Predicates

All predicates live in the `causality` module, and are called through the `causality::` prefix. A solver of
`Arg2pSolverFactory.causality()` also has them without the prefix: the module is one of its static libraries.

| Predicate | Meaning |
| --- | --- |
| `but_for(Cause, Effect)` | The counterfactual "but-for" test: succeeds if the effect would not hold had the cause been absent. The cause must be accepted: some argument for it IN in the grounded labelling of the theory. |
| `ness(Intervention, Cause, Effect)` | The NESS test, revised model. The cause must be accepted: some argument for it IN in the grounded labelling of the theory. `Intervention` **must be unbound**: it is bound to the intervention under which the cause is necessary for the effect. |
| `ness_intervention(Intervention, Effect)` | Checks the revised NESS condition for an intervention you provide. |
| `ness_original(Intervention, Cause, Effect)` | The published formulation of the NESS test. `Intervention` must be unbound. |
| `ness_original_intervention(Intervention, Effect)` | The published formulation, on a given intervention. |

`ness_original/3` and `ness_original_intervention/2` implement the model of Pisano, Prakken, Sartor and
Liepina (ICAIL 2025); `ness/3` and `ness_intervention/2` implement a revision to appear at ICAIL 2026. See
[References]({{% ref "/docs/references" %}}).

> [!WARNING]
> `ness/3` and `ness_original/3` raise a type error if their first argument is already bound. Always pass a
> fresh variable and read the intervention back from the solution.

---

## Running a query

A causality query is an ordinary goal: the module evaluates the theory of the solver by itself, so the query
needs no reset of the evaluation context first.

```kotlin
import it.unibo.tuprolog.core.Struct

val solution = solver.solve(Struct.parse("ness(X, exposure, disease)")).first()
println(solution.isYes)
```

With the DSL the same query reads:

```kotlin
import it.unibo.tuprolog.argumentation.core.dsl.arg2pScope

arg2pScope {
    solver.solve("ness"(X, "exposure", "disease")).first()
}
```

Through the API, the predicates are goals as well: `Arg2p.of(theory).solve("causality::ness(X, a, b)")`.

---

## A worked example

Two independent causes, each sufficient on its own — the classic case where the but-for test fails but NESS
succeeds:

```prolog
f_1 :=> fire_a.
f_2 :=> fire_b.
r_1 : fire_a => house_burns.
r_2 : fire_b => house_burns.
```

[Try it](https://tuprolog.github.io/arg2p-kt-web/?mode=structured&theory=f_1%20%3A%3D%3E%20fire_a.%0Af_2%20%3A%3D%3E%20fire_b.%0Ar_1%20%3A%20fire_a%20%3D%3E%20house_burns.%0Ar_2%20%3A%20fire_b%20%3D%3E%20house_burns.&query=causality%3A%3Aness%28X%2C%20fire_a%2C%20house_burns%29)
— the playground bundles the causality module, so the query runs there as it does on the JVM.

`but_for(fire_a, house_burns)` fails: removing `fire_a` leaves `fire_b`, and the house burns anyway.
`ness(X, fire_a, house_burns)` succeeds and binds `X` to the intervention it applied, here `[- fire_a]`: the
intervention negates the candidate cause, and the test reports the set under which that cause turns out to be
necessary for the effect.

---

## Notes on behaviour

- The causality solver runs its own inner evaluation with grounded semantics, whatever `argumentLabellingMode`
  says. The other flags are those of the outer solver, whether its settings or the theory state them (as
  `graphExtension`, `prologStrictCompatibility` or `metaConflicts`).
- `ness/3` and `but_for/2` only name accepted causes: a statement no argument concludes (the absence `-d` of a
  fact `d` never stated, say) or a rule identifier is not a cause for them, though `ness_original/3` may report
  it as one.
- Graphs computed during a causality query are cached, so repeated queries against the same theory and effect
  are considerably faster than the first one.
