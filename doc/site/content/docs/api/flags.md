---
title: Flags
weight: 20
aliases:
  - /docs/flags/
---

## Flags Reference

Flags are ordinary Prolog facts. The engine reads them from the knowledge base while it evaluates a theory,
so a flag can be set in three ways:

- **in the theory itself**, by writing the fact (e.g. `queryMode.`);
- **from the API**, through [`Flags`]({{% ref "/docs/api/kotlin#flags" %}}) (in Kotlin, JavaScript and Python), each
  flag with a method taking a value of its enumeration (`Semantics.PREFERRED`, `Profile.ABA`, ...);
- **from the IDE**, through the _Arg Flags_ tab.

A flag stated in the theory replaces the value the solver was given for it: a theory stating
`graphExtension(bp).` has only that extension, not the `standardPref` of `Flags.defaults()` as well.
`graphExtension(none).` states no extension, and a switch is turned off by `Switch(off).` (e.g.
`autoTransposition(off).`).

The [profile]({{% ref "/docs/profiles" %}}) of the theory, `graphBuildMode`, presets several flags at once. The
theory overrides those presets, and so does a value the solver is given that differs from the default of its flag.

The table below lists the flags, with their values in `Flags.defaults()`.

| Flag | Default | Accepted values |
| --- | --- | --- |
| `graphBuildMode(Mode)` | `aspic_plus` | `aspic_plus`, `aspic`, `aspic_defeasible`, `meta`, `aba`, `aba_plus`, `abstract`, `free`: see [profiles]({{% ref "/docs/profiles" %}}) |
| `graphBuilder(Mode)` | `ruleGraph` | `ruleGraph`, `abstractGraph`; set by the profile |
| `graphLanguage(Mode)` | none | `aspicLanguage`, `abaLanguage`, `defeasibleLanguage`; set by the profile |
| `conflictModel(Mode)` | `builtin` | `builtin`, `declared` |
| `argumentLabellingMode(Mode)` | `grounded` | see [argument semantics](#argument-semantics) |
| `statementLabellingMode(Mode)` | `statement` | `statement`, `statement_pass_through`, `statement_binary` |
| `graphExtension(Mode)` | `standardPref` | `standardPref`, `defeasiblePref`, `defeasibleAllPref`, `rebutRestriction`, `bp`, `abaPref` |
| `orderingPrinciple(Mode)` | `last` | `last`, `weakest` |
| `orderingComparator(Mode)` | `elitist` | `elitist`, `democrat`, `normal` |
| `queryMode` | enabled | the fact is present or absent |
| `autoTransposition` | enabled by `aspic_plus` | the fact is present or absent |
| `prologStrictCompatibility` | disabled | the fact is present or absent: the Prolog clauses of the theory are argued, see [plain Prolog]({{% ref "/docs/syntax#theories-in-plain-prolog" %}}) |
| `metaConflicts` | disabled | the fact is present or absent, see [meta-argumentation]({{% ref "/docs/modules/meta" %}}) |
| `metaRules` | disabled | the fact is present or absent, see [meta-argumentation]({{% ref "/docs/modules/meta" %}}) |
| `debug` | disabled | the fact is present or absent: `bp_grounded_partial` writes the steps of its labelling |
| `modulesPath(Path)` | `none` | a directory path or a base URL |

> [!NOTE]
> `graphBuildMode`, `argumentLabellingMode`, `statementLabellingMode`, `orderingPrinciple` and
> `orderingComparator` must always have a value. `Flags.defaults()` fills them in for you, so you only need to
> write them by hand when you build a theory that supplies its own flags (see
> [empty flag sets](#empty-flag-sets)).

---

## Graph construction

### `graphBuildMode(Mode)`

Selects the [profile]({{% ref "/docs/profiles" %}}) the theory is read in, which presets the flags its
framework needs. The default, `aspic_plus`, builds arguments, attacks and supports according to
ASPIC<sup>+</sup>, with preferences, restricted rebut and transposition.

### `graphBuilder(Mode)`

The module that builds the graph: `ruleGraph` from the rules of the theory, `abstractGraph` from the arguments
and attacks it states. The profile sets it.

### `graphLanguage(Mode)`

The languages the parser reads beyond the one of arg2p, each also checking the theory is written in it:
`aspicLanguage` (well-formed contraries), `abaLanguage` (assumptions and contraries; flat theories with strict
rules only) and `defeasibleLanguage` (defeasible rules only). The profile sets them.

### `conflictModel(Mode)`

`builtin` adds the built-in conflicts to those the theory declares: a literal and its negation, obligations and
permissions. `declared` keeps the declared ones only, as ABA does.

### `graphExtension(Mode)`

Post-processing steps applied to the graph once it has been built. Unlike the other flags, `graphExtension`
**may appear several times**: every occurrence adds an extension, and all of them are applied.

| Value | Effect |
| --- | --- |
| `standardPref` | ASPIC<sup>+</sup> static preferences. Attacks that do not succeed as defeats are removed according to the argument ordering. |
| `defeasiblePref` | Defeasible preferences, where a `sup/2` concluded by a rule counts as a preference. Supports comparisons that rest on a single superiority. |
| `defeasibleAllPref` | The richer model, which also supports comparators that need several superiorities at once. |
| `rebutRestriction` | Applies the *restricted rebut* constraint: an argument cannot rebut a conclusion drawn by a strict rule. |
| `abaPref` | The preferences between assumptions of ABA<sup>+</sup>, `sup(Stronger, Weaker)`: an attack from a weaker assumption is reversed. See [profiles]({{% ref "/docs/profiles#aba" %}}). |
| `bp` | Makes the burden of persuasion arguable: the burden is concluded by a rule, e.g. `r : [] => bp(something).`, instead of being declared as a fact. See [Meta-argumentation]({{% ref "/docs/modules/meta" %}}). |

The defeasible preference models and the meta-argumentation treatment of the burden of persuasion are
described in the papers listed under [References]({{% ref "/docs/references" %}}).

To disable preference handling altogether, pass an empty list of extensions
(`Flags.defaults().graphExtensions()`, which states `graphExtension(none).`) rather than omitting the flag.

> [!WARNING]
> In the IDE the _Unrestricted Rebut_ checkbox is the **inverse** of this flag: leaving it unticked adds
> `graphExtension(rebutRestriction)`. The _Meta Bp_ checkbox corresponds to `graphExtension(bp)`.

---

## Argument semantics

`argumentLabellingMode(Mode)` selects the semantics used to label arguments as **IN**, **OUT** or **UND**.

| Value | Semantics |
| --- | --- |
| `grounded` | Grounded semantics. The default. |
| `complete` | Dung's complete semantics. |
| `conflictfree` | Conflict-free labellings. |
| `admissible` | Admissible labellings. |
| `stronglyadmissible` | Strongly admissible labellings. |
| `preferred` | Preferred semantics. |
| `semistable` | Semi-stable semantics. |
| `stable` | Stable semantics. |
| `ideal` | Ideal semantics. |
| `eager` | Eager semantics. |
| `naive` | Naive semantics. |
| `stage` | Stage semantics. |
| `cf2` | CF2 semantics. |
| `stage2` | Stage2 semantics. |
| `bp_grounded` | Burden-of-persuasion grounded semantics. |
| `bp_grounded_partial` | Burden-of-persuasion partial semantics. |
| `bp_grounded_complete` | Burden-of-persuasion complete semantics. |

The burden-of-persuasion semantics implement the model of Calegari and Sartor; see
[References]({{% ref "/docs/references" %}}).

> [!NOTE]
> **One solution per labelling.** Semantics such as `preferred`, `stable`, `cf2` and `stage2` admit
> several labellings. The engine returns them as **distinct solutions**: iterate over the solution sequence to
> see them all, instead of taking only the first one.

---

## Statement semantics

`statementLabellingMode(Mode)` decides how argument labels are propagated to the statements they conclude.

| Value | Effect |
| --- | --- |
| `statement` | Standard labelling: a statement is IN if some IN argument concludes it, otherwise OUT if some OUT argument concludes it, otherwise UND. |
| `statement_pass_through` | Propagates argument labels directly, without re-deriving the statement status. |
| `statement_binary` | Two-valued labelling: a statement is IN if some IN argument concludes it, OUT otherwise (merging OUT and UND). |

---

## Argument ordering

These two flags are used together whenever preferences are enabled, to decide when one argument is stronger
than another.

### `orderingPrinciple(Mode)`

Which part of an argument is compared:

- `last` — compares only the *last* defeasible rules used in the argument;
- `weakest` — compares *all* the defeasible rules, and the argument is as strong as its weakest link.

### `orderingComparator(Mode)`

How the two sets selected by the ordering principle are compared:

- `elitist` — an argument is weaker if *some* of its rules is weaker than *all* the rules of the other;
- `democrat` — an argument is weaker if *each* of its rules is weaker than *some* rule of the other;
- `normal` — an argument is weaker if some of its last rules is weaker than some last rule of the other.

A non-empty set is always weaker than an empty one: an argument with no defeasible rule (or premise) to compare
is stronger than one with some, whatever the preferences. `elitist` and `democrat` are the set comparisons of
Modgil and Prakken, combined with either principle; `normal` is the attack relation of Dung and Thang, defined on
the last rules, so it only goes with `orderingPrinciple(last)`: with `weakest` the evaluation raises a domain
error. See [References]({{% ref "/docs/references#preferences" %}}).

> [!NOTE]
> The normal attack relation knows rules only. For `orderingPrinciple(last)` with `orderingComparator(normal)`,
> write the defeasible facts as rules with an empty body (`r1 : [] => a.`) rather than as premises (`f1 :=> a.`):
> a premise is not one of the last rules of its arguments, so preferences on it are not weighed as Dung and Thang
> define them.

---

## Evaluation behaviour

### `queryMode`

Enables the goal-directed (structured) evaluation used by [`answerQuery/4`]({{% ref "/docs/api/engine" %}}).
Instead of building the whole graph, the engine builds only the sub-graph relevant to the query, which is
considerably faster on large theories. It is **enabled** in `Flags.defaults()`.

### `autoTransposition`

Closes the theory under transposition: for every strict rule the contrapositive variants are added
automatically, over negations and conflicts declared both ways. The rationality postulates require it, and the
profiles `aspic_plus` and `aspic` turn it on; `autoTransposition(off).` turns it off.

```prolog
autoTransposition.
r1 : a, b -> c.     % also yields  -c, b -> -a.  and  -c, a -> -b.
```

### `modulesPath(Path)`

Where [`call_module/2`]({{% ref "/docs/modules" %}}) looks when it loads an external `.pl` module by name.
It defaults to `none`, which means no module can be resolved by name. It can be a local directory or a base
URL:

```prolog
modulesPath('/home/me/theories').
modulesPath('https://example.org/theories').
```

See [remote modules]({{% ref "/docs/modules" %}}) for the trust implications of loading a theory over the
network.

---

## Flags of the theory alone

`naturalTerms` is read only from the theory text; `Flags` has no method for it (`debug` has one, `debug(true)`).
Write the facts directly in your theory:

| Flag | Meaning |
| --- | --- |
| `naturalTerms` | Enables normalisation of natural-language-like terms while parsing rules. |
| `debug` | `bp_grounded_partial` writes the steps of its labelling. |

---

## Empty flag sets

`Flags.none()` disables the injection of *every* flag fact. The theory then has to declare all the required
flags itself:

```kotlin
val arg2p = Arg2p.of(
    """
        argumentLabellingMode(grounded).
        statementLabellingMode(statement).
        orderingPrinciple(last).
        orderingComparator(elitist).
        graphExtension(standardPref).
        queryMode.

        f1 :=> d.
        f2 :=> -d.
    """.trimIndent(),
    Flags.none(),
)
```

`Flags.parse(text)` states only the flags of its Prolog facts as well. With the 2P-Kt integration, the same is
`FlagsBuilder().empty(true)`.
