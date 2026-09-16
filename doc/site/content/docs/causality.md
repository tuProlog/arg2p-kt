---
title: Causality
weight: 26
---

## Causality

The causality solver answers questions of the form *"was this the cause of that?"* over an argumentation
theory. It is a separate module, built on top of the core engine, and it adds a small set of Prolog predicates
implementing counterfactual and NESS causal tests.

It is available as the Maven module `causality-solver` and as the npm package
`@tuprolog/arg2p-causality-solver`. It is also bundled in the
[IDE]({{% ref "/docs/getting-started" %}}) and in the JavaScript bridge, so in those two environments it is
ready to use with no extra setup.

---

## Getting a causality solver

From Kotlin, the entry point is the `causality` extension on `Arg2pSolverFactory`:

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

All predicates live in the `causality` module. As with the rest of the engine, they can be called directly or
through the `causality::` prefix.

| Predicate | Meaning |
| --- | --- |
| `but_for(Cause, Effect)` | The counterfactual "but-for" test: succeeds if the effect would not hold had the cause been absent. |
| `ness(Intervention, Cause, Effect)` | The NESS test, revised model. `Intervention` **must be unbound**: it is bound to the intervention under which the cause is necessary for the effect. |
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

Every causality query must start from a clean evaluation context, so prefix it with `context_reset`:

```kotlin
import it.unibo.tuprolog.core.Struct

val solution = solver.solve(Struct.parse("context_reset, ness(X, exposure, disease)")).first()
println(solution.isYes)
```

With the DSL the same query reads:

```kotlin
import it.unibo.tuprolog.argumentation.core.dsl.arg2pScope

arg2pScope {
    solver.solve("context_reset" and "ness"(X, "exposure", "disease")).first()
}
```

From the [JavaScript bridge]({{% ref "/docs/run" %}}), pass the query as text:

```js
arg2p.solve('causality::ness(X, a, b)', theory, flags, _ => { })
```

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

`but_for(fire_a, house_burns)` fails: removing `fire_a` leaves `fire_b`, and the house burns anyway.
`ness(X, fire_a, house_burns)` succeeds, binding `X` to the intervention in which `fire_b` is absent, where
`fire_a` *is* necessary for the outcome.

---

## Notes on behaviour

- The causality solver runs its own inner evaluation with grounded semantics and no graph extensions,
  independently of the flags you set for the outer solver.
- Graphs computed during a causality query are cached, so repeated queries against the same theory and effect
  are considerably faster than the first one.
