---
title: Meta-argumentation
weight: 29
---

## Meta-argumentation

Ordinarily, a theory argues about *statements*: rules derive conclusions, and conclusions conflict. The meta
features let a theory argue about the argumentation machinery itself — about which rules apply, what counts as
a conflict, which rule prevails, and who carries the burden of persuasion.

Each of them turns something that is normally fixed (declared as a fact, or built into the engine) into
something a rule can conclude, and therefore something another argument can attack.

| Feature | Normally | As a meta feature |
| --- | --- | --- |
| Rules | a rule always applies | `metaRules` — a rule applies only if `applicable/1` is argued |
| Conflicts | `conflict/2` facts and built-in conflicts | `metaConflicts` — rules conclude `conflict/2` |
| Preferences | `sup/2` facts | `graphExtension(defeasiblePref)` — rules conclude `sup/2` |
| Burden of persuasion | `bp/1` facts read by the `bp_grounded` semantics | `graphExtension(bp)` — rules conclude `bp/1` |

> [!IMPORTANT]
> The meta features are applied while the argumentation graph is built, which the goal-directed evaluation
> skips. Turn **`queryMode` off** when you use them, otherwise they are silently ignored.

---

## Arguable rules

`metaRules` adds an implicit premise `applicable(RuleName)` to every rule. A rule then fires only if something
argues that it is applicable, which makes rule application itself defeasible.

```prolog
metaRules.

r1 : p => q.
f1 :=> p.
```

`q` is **not** derived: nothing argues that `r1` applies. Supplying the missing premise brings it back:

```prolog
metaRules.

r1 : p => q.
f1 :=> p.
f2 :=> applicable(r1).
```

```prolog
?- arg2p::buildLabelSets(In, Out, Und).

In = [[applicable(r1)], [q], [p]]
```

[Try it](https://tuprolog.github.io/arg2p-kt-web/?mode=structured&theory=metaRules.%0Ar1%20%3A%20p%20%3D%3E%20q.%0Af1%20%3A%3D%3E%20p.%0Af2%20%3A%3D%3E%20applicable%28r1%29.&query=arg2p%3A%3AbuildLabelSets%28In%2C%20Out%2C%20Und%29&flags=graphBuildMode%28standard_af%29.%0AstatementLabellingMode%28statement%29.%0AargumentLabellingMode%28grounded%29.%0AorderingPrinciple%28last%29.%0AorderingComparator%28elitist%29.%0AgraphExtension%28standardPref%29.)
— remove the `applicable/1` premise and `q` disappears.

Because `applicable/1` is an ordinary conclusion, a second rule can attack it — for instance concluding
`-applicable(r1)` when the rule's conditions of use are not met.

---

## Arguable conflicts

Without `metaConflicts`, conflicts come from the built-in table (`a` versus `-a`, the deontic pairs) plus any
[`conflict/2` facts]({{% ref "/docs/syntax" %}}) in the theory. With `metaConflicts`, they come from
*arguments*: whatever an argument concludes as `conflict/2` becomes a conflict.

```prolog
metaConflicts.

r0 : [] => conflict(a, b).
f1 :=> a.
f2 :=> b.
```

```prolog
?- arg2p::buildLabelSets(In, Out, Und).

In  = [[conflict(a, b)], [a]]
Out = [[b]]
```

Without `r0`, `a` and `b` coexist happily; the meta conflict is what sets them against each other.

[Try it](https://tuprolog.github.io/arg2p-kt-web/?mode=structured&theory=metaConflicts.%0Ar0%20%3A%20%5B%5D%20%3D%3E%20conflict%28a%2C%20b%29.%0Af1%20%3A%3D%3E%20a.%0Af2%20%3A%3D%3E%20b.&query=arg2p%3A%3AbuildLabelSets%28In%2C%20Out%2C%20Und%29&flags=graphBuildMode%28standard_af%29.%0AstatementLabellingMode%28statement%29.%0AargumentLabellingMode%28grounded%29.%0AorderingPrinciple%28last%29.%0AorderingComparator%28elitist%29.%0AgraphExtension%28standardPref%29.)

> [!WARNING]
> The conclusion has to name the two statements directly — `conflict(a, b)`, not `conflict([a], [b])`.
> A rule concluding the list form parses fine and is simply never matched.

Since the conflict is a conclusion, it can be attacked: an argument for `-conflict(a, b)` removes the clash
again.

---

## Arguable preferences

`sup/2` facts are static. To let rules *argue* about priority, enable one of the defeasible preference
models:

```prolog
graphExtension(defeasiblePref).

r1 : p => q.
r2 : s => -q.
f1 :=> p.
f2 :=> s.
rs : [] => sup(r1, r2).
```

```prolog
?- arg2p::buildLabelSets(In, Out, Und).

In  = [[sup(r1, r2)], [s], [q], [p]]
Out = [[- q]]
```

`q` prevails because an argument establishes that `r1` outranks `r2`. Under the plain
`graphExtension(standardPref)` the same theory leaves both conclusions undecided: static preference handling
reads `sup/2` facts only, and ignores a `sup/2` that is merely concluded.

[Try it](https://tuprolog.github.io/arg2p-kt-web/?mode=structured&theory=r1%20%3A%20p%20%3D%3E%20q.%0Ar2%20%3A%20s%20%3D%3E%20-q.%0Af1%20%3A%3D%3E%20p.%0Af2%20%3A%3D%3E%20s.%0Ars%20%3A%20%5B%5D%20%3D%3E%20sup%28r1%2C%20r2%29.&query=arg2p%3A%3AbuildLabelSets%28In%2C%20Out%2C%20Und%29&flags=graphBuildMode%28standard_af%29.%0AstatementLabellingMode%28statement%29.%0AargumentLabellingMode%28grounded%29.%0AorderingPrinciple%28last%29.%0AorderingComparator%28elitist%29.%0AgraphExtension%28standardPref%29.%0AgraphExtension%28defeasiblePref%29.)
— switch the extension to `standardPref` alone and the preference stops applying.

`defeasiblePref` covers comparisons that rest on a **single** superiority. When the comparator has to weigh
several superiorities at once — as the elitist and democrat comparisons do over sets of rules — use
`graphExtension(defeasibleAllPref)` instead, which also exposes the preference as a `preference/1` statement.
The two models are described in the CILC paper listed under [References]({{% ref "/docs/references" %}}).

---

## Arguable burden of persuasion

This is the one most easily confused with a semantics, so it is worth stating the difference plainly.

**As a fact**, the burden is input to the labelling: the `bp_grounded` family reads `bp/1` facts and uses them
to break deadlocks.

```prolog
argumentLabellingMode(bp_grounded).

bp(guilty).
r1 : evidence => guilty.
r2 : alibi => -guilty.
f1 :=> evidence.
f2 :=> alibi.
```

```prolog
?- arg2p::buildLabelSets(In, Out, Und).

Out = [[guilty]]
```

**As a rule conclusion**, with `graphExtension(bp)`, the burden becomes part of the graph: the engine builds an
argument for `bp(guilty)` and an artificial argument for `-burdmet([guilty])`, recording that the burden has
not been met, and attacks the burdened argument with it.

```prolog
graphExtension(bp).

r0 : [] => bp(guilty).
r1 : evidence => guilty.
r2 : alibi => -guilty.
f1 :=> evidence.
f2 :=> alibi.
```

```prolog
?- arg2p::buildLabelSets(In, Out, Und).

In  = [[bp(guilty)], [- burdmet([guilty])], [- guilty], [evidence], [alibi]]
Out = [[guilty]]
```

The outcome is the same as the fact form — `guilty` is **OUT** — which is the point: the two routes express the
same burden, and only differ in whether it can be argued about.

[Try it](https://tuprolog.github.io/arg2p-kt-web/?mode=structured&theory=r0%20%3A%20%5B%5D%20%3D%3E%20bp%28guilty%29.%0Ar1%20%3A%20evidence%20%3D%3E%20guilty.%0Ar2%20%3A%20alibi%20%3D%3E%20-guilty.%0Af1%20%3A%3D%3E%20evidence.%0Af2%20%3A%3D%3E%20alibi.&query=arg2p%3A%3AbuildLabelSets%28In%2C%20Out%2C%20Und%29&flags=graphBuildMode%28standard_af%29.%0AstatementLabellingMode%28statement%29.%0AargumentLabellingMode%28grounded%29.%0AorderingPrinciple%28last%29.%0AorderingComparator%28elitist%29.%0AgraphExtension%28bp%29.%0AgraphExtension%28standardPref%29.)

And it can. Add a reason to doubt the allocation:

```prolog
rx : doubt => -bp(guilty).
f0 :=> doubt.
```

```prolog
?- arg2p::buildLabelSets(In, Out, Und).

Und = [[bp(guilty)], [- burdmet([guilty])], [- bp(guilty)], [- guilty], [guilty]]
```

Now that it is disputed whether the burden falls on `guilty` at all, the burden argument, its `-burdmet/1`
consequence and the burdened statement are all undecided: the theory argues about *who has to prove what*,
not merely about what is provable.

[Try it](https://tuprolog.github.io/arg2p-kt-web/?mode=structured&theory=r0%20%3A%20%5B%5D%20%3D%3E%20bp%28guilty%29.%0Ar1%20%3A%20evidence%20%3D%3E%20guilty.%0Ar2%20%3A%20alibi%20%3D%3E%20-guilty.%0Af1%20%3A%3D%3E%20evidence.%0Af2%20%3A%3D%3E%20alibi.%0Arx%20%3A%20doubt%20%3D%3E%20-bp%28guilty%29.%0Af0%20%3A%3D%3E%20doubt.&query=arg2p%3A%3AbuildLabelSets%28In%2C%20Out%2C%20Und%29&flags=graphBuildMode%28standard_af%29.%0AstatementLabellingMode%28statement%29.%0AargumentLabellingMode%28grounded%29.%0AorderingPrinciple%28last%29.%0AorderingComparator%28elitist%29.%0AgraphExtension%28bp%29.%0AgraphExtension%28standardPref%29.)
— the contested version, for comparison.

The meta-argumentation account of the burden of persuasion is described in Pisano, Calegari, Omicini and
Sartor; see [References]({{% ref "/docs/references" %}}).
