---
title: Profiles
weight: 22
---

## Profiles

A **profile** names the framework a theory is read in. It is the value of `graphBuildMode`, and a shortcut for
the flags that framework needs: choosing one presets them.

```prolog
graphBuildMode(aba).
```

A flag the theory states overrides the preset of its profile, and so does a flag the solver is given with a value
other than its default. `graphExtension(none).` clears the extensions a profile presets, and `autoTransposition(off).`
turns off a switch it turns on.

| Profile | Reads the theory as | Presets |
| --- | --- | --- |
| `aspic_plus` | ASPIC<sup>+</sup>, the **default** | `standardPref`, `rebutRestriction`, `autoTransposition`, well-formed contraries |
| `aspic` | ASPIC<sup>+</sup> with no preferences | `rebutRestriction`, `autoTransposition`, well-formed contraries |
| `aspic_defeasible` | the defeasible part of ASPIC<sup>+</sup> | `standardPref`, `orderingPrinciple(last)`, `orderingComparator(normal)`, defeasible rules only |
| `meta` | ASPIC<sup>+</sup> reasoning about its rules and conflicts | `defeasiblePref`, `metaRules`, `metaConflicts` |
| `aba` | flat ABA | assumptions and contraries, strict rules only, declared conflicts only |
| `aba_plus` | flat ABA<sup>+</sup> under Weak Contraposition | as `aba`, with `abaPref` |
| `abstract` | Dung frameworks | arguments and attacks |
| `free` | nothing in particular | none: every flag as the solver and the theory state it |

### ASPIC<sup>+</sup>

`aspic_plus` is the framework of Modgil and Prakken: strict and defeasible rules, axioms and ordinary premises,
preferences, restricted rebut, and the strict rules closed under transposition, as the rationality postulates
require. The theory must be **well formed**: a contrary, a conflict declared one way only (`conflict([X], [Y])`
with no `conflict([Y], [X])`), may not target an axiom or the conclusion of a strict rule, which no attack could
reach. A theory that breaks this is refused. Negations, and conflicts declared both ways, are contradictories,
and transposition closes the strict rules over them.

`aspic_defeasible` takes defeasible rules alone, facts written as rules with no premise (`r : [] => a.`). It refuses
strict rules, axioms and premises.

### ABA

`aba` reads assumption-based argumentation (Bondarenko et al.; Dung, Kowalski and Toni) as the instance of
ASPIC<sup>+</sup> it is:

```prolog
graphBuildMode(aba).

assumption(a).
assumption(b).
contrary(a, x).     % an argument for x attacks every argument resting on a

r1 : b -> x.
r2 : a -> y.
```

Rules are strict. A contrary attacks one way only, and there is no other conflict, not even negation.

`aba_plus` adds preferences between assumptions, `sup(Stronger, Weaker)`, as in ABA<sup>+</sup> (Čyras and
Toni). An attack from an argument resting on an assumption weaker than the one it attacks is **reversed**: the
attacked assumption attacks every argument that rests on all the assumptions of the attacker.

#### Limits

The arguments of arg2p give the semantics of ABA, defined on sets of assumptions, only for some theories. The
profiles refuse the others, with an error explaining why:

- **Non-flat ABA**, where a rule concludes an assumption, is refused by `aba` and `aba_plus`. Its semantics would
  need attacks between sets of assumptions (bipolar set-based frameworks) that arguments do not express.
- **ABA<sup>+</sup> without Weak Contraposition** is refused by `aba_plus`. Weak Contraposition (Čyras and Toni,
  Axiom 4) says that if a set of assumptions `A` derives the contrary of `β` and some of `A` are weaker than `β`,
  then for one `α` of the weakest of them, `A` without `α` but with `β` derives the contrary of `α`.

Here `a1` is weaker than `a0`, and `{a0, a1}` derives the contrary of `a0`:

```prolog
graphBuildMode(aba_plus).

assumption(a0).
assumption(a1).
contrary(a0, x).
contrary(a1, y).
sup(a0, a1).

r0 : a0, a1 -> x.
```

ABA<sup>+</sup> reverses the attack: `{a0}` attacks `{a0, a1}`. Its preferred extensions are `{a0}` and `{a1}`,
which cannot stand together, though no argument attacks another. Adding `r1 : a0 -> y.` satisfies Weak
Contraposition: `a0` then attacks `a1`, and the arguments give the same extensions as the sets.

A future labelling over attacks between sets of assumptions could handle these theories.

### Abstract frameworks

`abstract` reads a Dung framework: `a.` (or `arg(a)`) an argument, `a -> b.` (or `att(a, b)`) an attack, and
`a <-> b.` a mutual one. Prolog clauses can derive them:

```prolog
graphBuildMode(abstract).

beats(x, y).
beats(y, z).
X -> Y :- beats(X, Y).
```
