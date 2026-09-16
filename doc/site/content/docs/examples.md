---
title: Examples
weight: 38
---

## Examples

Complete theories you can paste into the [IDE or the Playground]({{% ref "/docs/getting-started" %}}) and run
as they are. Each one isolates a single feature of the framework.

Most of them carry a **Try it** link that opens the example already loaded in the
[Web Playground](https://tuprolog.github.io/arg2p-kt-web/), so you can run it without copying anything.

---

## Conflicting premises

The smallest interesting theory: two premises supporting opposite conclusions.

```prolog
f1 :=> d.
f2 :=> -d.
```

```prolog
?- arg2p::solve(d, Res).

Res = [und(d)]
```

Both conclusions are **UND**. Neither argument defeats the other, so neither can be accepted.

[Try it](https://tuprolog.github.io/arg2p-kt-web/?mode=structured&theory=f1%20%3A%3D%3E%20d.%0Af2%20%3A%3D%3E%20-d.&query=arg2p%3A%3Asolve%28d%2C%20Res%29)

---

## Resolving a conflict with preferences

Adding a superiority relation breaks the tie:

```prolog
f1 :=> d.
f2 :=> -d.
sup(f1, f2).
```

```prolog
?- arg2p::solve(d, Res).

Res = [in(d)]
```

`d` is now **IN** and `-d` is **OUT**. Preference handling is active by default through
`graphExtension(standardPref)`; see [Flags]({{% ref "/docs/flags" %}}) for the other preference models.

[Try it](https://tuprolog.github.io/arg2p-kt-web/?mode=structured&theory=f1%20%3A%3D%3E%20d.%0Af2%20%3A%3D%3E%20-d.%0Asup%28f1%2C%20f2%29.&query=arg2p%3A%3Asolve%28d%2C%20Res%29)

---

## Strict rules and axioms

Strict rules (`->`) cannot be defeated, and axioms (`:->`) are premises that cannot be questioned:

```prolog
a1 :-> bird(tweety).
r1 : bird(X) -> animal(X).
r2 : bird(X) => flies(X).
r3 : penguin(X) => -flies(X).
```

`animal(tweety)` follows strictly, so no argument can attack it. `flies(tweety)`, derived defeasibly, remains
open to attack.

[Try it](https://tuprolog.github.io/arg2p-kt-web/?mode=structured&theory=a1%20%3A-%3E%20bird%28tweety%29.%0Ar1%20%3A%20bird%28X%29%20-%3E%20animal%28X%29.%0Ar2%20%3A%20bird%28X%29%20%3D%3E%20flies%28X%29.%0Ar3%20%3A%20penguin%28X%29%20%3D%3E%20-flies%28X%29.&query=arg2p%3A%3Asolve%28animal%28tweety%29%2C%20Res%29)

---

## Exceptions with weak negation

Weak negation expresses "unless we know otherwise":

```prolog
r1 : bird(X), ~(penguin(X)) => flies(X).
f1 :=> bird(tweety).
f2 :=> penguin(tweety).
```

The premise `~(penguin(tweety))` fails, so `r1` never applies to Tweety and `flies(tweety)` is not concluded.

[Try it](https://tuprolog.github.io/arg2p-kt-web/?mode=structured&theory=r1%20%3A%20bird%28X%29%2C%20%7E%28penguin%28X%29%29%20%3D%3E%20flies%28X%29.%0Af1%20%3A%3D%3E%20bird%28tweety%29.%0Af2%20%3A%3D%3E%20penguin%28tweety%29.&query=arg2p%3A%3Asolve%28flies%28tweety%29%2C%20Res%29)

---

## Undercutting a rule

An undercut attacks the *applicability* of a rule rather than its conclusion:

```prolog
r1 : witness_says(X) => reliable(X).
r2 : unreliable(witness) => undercut(r1).
f1 :=> witness_says(story).
f2 :=> unreliable(witness).
```

`r2` disables `r1`, so `reliable(story)` is not accepted, even though nothing argues against the story
directly.

[Try it](https://tuprolog.github.io/arg2p-kt-web/?mode=structured&theory=r1%20%3A%20witness_says%28X%29%20%3D%3E%20reliable%28X%29.%0Ar2%20%3A%20unreliable%28witness%29%20%3D%3E%20undercut%28r1%29.%0Af1%20%3A%3D%3E%20witness_says%28story%29.%0Af2%20%3A%3D%3E%20unreliable%28witness%29.&query=arg2p%3A%3Asolve%28reliable%28story%29%2C%20Res%29)

---

## Obligations and violations

The deontic operators express prohibitions and the violations that follow from breaking them:

```prolog
f1 :=> o(-enter).
f2 :=> enter.
v_rule : o(-enter), enter => violation.
```

The obligation not to enter, together with the fact of entering, yields `violation`.

[Try it](https://tuprolog.github.io/arg2p-kt-web/?mode=structured&theory=f1%20%3A%3D%3E%20o%28-enter%29.%0Af2%20%3A%3D%3E%20enter.%0Av_rule%20%3A%20o%28-enter%29%2C%20enter%20%3D%3E%20violation.&query=arg2p%3A%3Asolve%28violation%2C%20Res%29)

---

## Querying a single goal

Instead of computing every labelling, ask about one conclusion. This uses the goal-directed evaluation enabled
by `queryMode`:

```prolog
f1 :=> d.
f2 :=> -d.
```

```prolog
?- arg2p::answerQuery(d, In, Out, Und).
```

Only the part of the graph relevant to `d` is built, which matters on large theories. When the three separate
sets are not needed, `arg2p::solve(d, Res)` returns the same information as a single list.

[Try it](https://tuprolog.github.io/arg2p-kt-web/?mode=structured&theory=f1%20%3A%3D%3E%20d.%0Af2%20%3A%3D%3E%20-d.&query=arg2p%3A%3AanswerQuery%28d%2C%20In%2C%20Out%2C%20Und%29)

---

## Choosing a different semantics

Grounded semantics returns exactly one labelling. Semantics such as `preferred` may return several, one per
solution:

```prolog
argumentLabellingMode(preferred).

f1 :=> a.
f2 :=> -a.
```

Asking for all solutions yields two labellings: one accepting `a`, one accepting `-a`. In Kotlin, iterate over
the sequence rather than taking `first()`.

[Try it](https://tuprolog.github.io/arg2p-kt-web/?mode=structured&theory=f1%20%3A%3D%3E%20a.%0Af2%20%3A%3D%3E%20-a.&query=arg2p%3A%3Asolve&flags=graphBuildMode%28standard_af%29.%0AstatementLabellingMode%28statement%29.%0AargumentLabellingMode%28preferred%29.%0AorderingPrinciple%28last%29.%0AorderingComparator%28elitist%29.%0AgraphExtension%28standardPref%29.%0AqueryMode.)
— the link carries the whole flag block, since a shared link replaces the flags editor wholesale.

---

## Evaluating an abstract framework

When you already have arguments and attacks and do not need to build them from rules:

```prolog
?- abstract::solve([a, b, c], [(a, b), (b, c)], I, O, U).

I = [a, c],
O = [b],
U = [].
```

See [Abstract Evaluation]({{% ref "/docs/modules/abstract" %}}).

[Try it](https://tuprolog.github.io/arg2p-kt-web/?mode=abstract&arguments=a%2Cb%2Cc&attacks=a-b%2Cb-c) — this
one opens the playground in **Abstract** mode with the framework already drawn.

---

## Burden of persuasion

A statement carrying the burden of persuasion has to be *established*: when the arguments for and against it
cancel out, it fails instead of staying undecided. The burden is declared with `bp/1` and evaluated by the
`bp_grounded` family of semantics:

```prolog
argumentLabellingMode(bp_grounded).

bp(guilty).
r1 : evidence => guilty.
r2 : alibi => -guilty.
f1 :=> evidence.
f2 :=> alibi.
```

```prolog
?- arg2p::solve(guilty, Res).

Res = [out(guilty)]
```

The evidence and the alibi defeat each other, so without a burden `guilty` would be **UND**. The burden
resolves the deadlock against the party carrying it, and `guilty` comes out **OUT**.

[Try it](https://tuprolog.github.io/arg2p-kt-web/?mode=structured&theory=bp%28guilty%29.%0Ar1%20%3A%20evidence%20%3D%3E%20guilty.%0Ar2%20%3A%20alibi%20%3D%3E%20-guilty.%0Af1%20%3A%3D%3E%20evidence.%0Af2%20%3A%3D%3E%20alibi.&query=arg2p%3A%3Asolve%28guilty%2C%20Res%29&flags=graphBuildMode%28standard_af%29.%0AstatementLabellingMode%28statement%29.%0AargumentLabellingMode%28bp_grounded%29.%0AorderingPrinciple%28last%29.%0AorderingComparator%28elitist%29.%0AgraphExtension%28standardPref%29.%0AqueryMode.)

---

## Causal questions

With the [causality module]({{% ref "/docs/modules/causality" %}}), you can ask whether something caused something
else:

```prolog
f_1 :=> fire_a.
f_2 :=> fire_b.
r_1 : fire_a => house_burns.
r_2 : fire_b => house_burns.
```

```prolog
?- context_reset, causality::ness(X, fire_a, house_burns).
```

[Try it](https://tuprolog.github.io/arg2p-kt-web/?mode=structured&theory=f_1%20%3A%3D%3E%20fire_a.%0Af_2%20%3A%3D%3E%20fire_b.%0Ar_1%20%3A%20fire_a%20%3D%3E%20house_burns.%0Ar_2%20%3A%20fire_b%20%3D%3E%20house_burns.&query=context_reset%2C%20causality%3A%3Aness%28X%2C%20fire_a%2C%20house_burns%29)

The but-for test fails here — the house burns either way — while the NESS test succeeds, binding `X` to the
intervention under which `fire_a` really is necessary.
