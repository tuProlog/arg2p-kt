---
title: Troubleshooting
weight: 39
---

## Troubleshooting

Common mistakes, and what they look like when they happen.

---

## The evaluation never produces a graph

**Symptom.** A query silently fails, or `graph()` throws *"couldn't find a graph"*.

Most often one of the required flags is missing. `graphBuildMode`, `argumentLabellingMode`,
`statementLabellingMode`, `orderingPrinciple` and `orderingComparator` must all have a value. `FlagsBuilder()`
supplies them, but a theory evaluated with `FlagsBuilder().empty(true)` — or a flags string passed to the
[JavaScript bridge]({{% ref "/docs/javascript" %}}) — has to declare every one of them itself.

The other frequent cause is reading the graph before running a query: `graph()` mines the *current* evaluation
context, so a query such as `arg2p::solve` has to succeed first.

---

## A hand-built solver behaves strangely

**Symptom.** Queries fail where they should succeed, or variables come back unbound.

A solver assembled directly with `ClassicSolverFactory` needs two flags:

```kotlin
flags = FlagStore.DEFAULT
    .set(Unknown, Unknown.FAIL)
    .set(TrackVariables, TrackVariables.ON)
```

Without `Unknown.FAIL`, unknown predicates raise errors instead of failing, which breaks the engine's internal
negation-as-failure. Without `TrackVariables.ON`, variable names are lost and results cannot be mined.
`Arg2pSolverFactory` sets both for you.

---

## `Theory.parse` rejects the theory

**Symptom.** A syntax error on a rule that looks perfectly valid.

Arg2P adds operators (`=>`, `:=>`, `:->`, `:`) that the default parser does not know. Parse with the Arg2P
operator set:

```kotlin
val theory = Theory.parse(theoryText, Arg2pSolver.default().operators())
```

---

## A goal using `::` cannot be parsed

**Symptom.** `Struct.parse("arg2p::solve(d, Res)")` raises a syntax error.

`::` is an operator defined by Arg2P, so the parser has to be told about it:

```kotlin
Struct.parse("arg2p::solve(d, Res)", solver.operators)
```

The same applies to `Theory.parse` — see [Kotlin API]({{% ref "/docs/kotlin-api" %}}).

---

## An invalid flag value is ignored

**Symptom.** You set a semantics and nothing changes.

Flag values are module identifiers, and an unknown value simply matches no module. Check the spelling against
the [Flags Reference]({{% ref "/docs/flags" %}}) — for instance the grounded semantics is `grounded`, and
there is no `grounded_hash` mode.

---

## Only one labelling comes back

**Symptom.** Under `preferred`, `stable`, `ideal`, `cf2` or `stage2` you see a single result.

These semantics produce one solution *per* labelling. Taking `first()` on the sequence, or reading only the
first Prolog solution, discards the rest. Iterate over all solutions instead.

---

## A causality query fails or raises a type error

Two rules apply to the [causality module]({{% ref "/docs/causality" %}}):

- every query must be preceded by `context_reset`, otherwise it runs against a stale context;
- `ness/3` and `ness_original/3` require their **first argument to be unbound**. Passing a concrete
  intervention raises a type error; use `ness_intervention/2` for that instead.

---

## A program using the distributed solver does not terminate

The [actor solver]({{% ref "/docs/actor-solver" %}}) runs non-daemon threads for the cluster. Call `leave`
when the work is done, or the JVM will stay alive.

---

## Weak negation is rejected in a conclusion

Weak negation `~(Term)` is only allowed in the body of a rule. Strong negation `-Term` is the one that may
appear in a conclusion, and it cannot be nested.

---

## Results change when preferences are enabled

This is expected: with `graphExtension(standardPref)` an attack only succeeds as a defeat if the attacker is
not weaker than its target, so some attacks disappear. The outcome also depends on `orderingPrinciple` and
`orderingComparator`. To evaluate without any preference handling, pass an empty extension list
(`FlagsBuilder(graphExtensions = emptyList())`).

---

## Rationality postulates are not satisfied

Closure under transposition is off by default. Some postulates require it:

```prolog
autoTransposition.
```

Note that it enlarges the theory, since each strict rule gains its contrapositive variants.
