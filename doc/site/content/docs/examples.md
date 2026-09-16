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

See [Abstract Evaluation]({{% ref "/docs/abstract" %}}).

[Try it](https://tuprolog.github.io/arg2p-kt-web/?mode=abstract&arguments=a%2Cb%2Cc&attacks=a-b%2Cb-c) — this
one opens the playground in **Abstract** mode with the framework already drawn.

---

## Burden of persuasion

With the `bp` extension, the burden of persuasion is expressed inside the rules and evaluated by a dedicated
semantics:

```prolog
graphExtension(bp).
argumentLabellingMode(bp_grounded).

r0 : [] => bp(guilty).
r1 : evidence => guilty.
r2 : alibi => -guilty.
f1 :=> evidence.
f2 :=> alibi.
```

The burden rests on `guilty`: where the evidence and the alibi cancel out, the statement carrying the burden
is the one that fails.

---

## Causal questions

With the [causality module]({{% ref "/docs/causality" %}}), you can ask whether something caused something
else:

```prolog
f_1 :=> fire_a.
f_2 :=> fire_b.
r_1 : fire_a => house_burns.
r_2 : fire_b => house_burns.
```

```prolog
?- context_reset, ness(X, fire_a, house_burns).
```

The but-for test fails here — the house burns either way — while the NESS test succeeds, binding `X` to the
intervention under which `fire_a` really is necessary.
