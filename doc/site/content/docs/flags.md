---
title: Flags Reference
weight: 32
---

## Flags Reference

Flags are ordinary Prolog facts. The engine reads them from the knowledge base while it evaluates a theory,
so a flag can be set in three equivalent ways:

- **in the theory itself**, by writing the fact (e.g. `queryMode.`);
- **from Kotlin**, through [`FlagsBuilder`]({{% ref "/docs/kotlin-api" %}});
- **from the IDE**, through the _Arg Flags_ tab.

The table below is the complete list. Defaults are the ones applied by `FlagsBuilder()`.

| Flag | Default | Accepted values |
| --- | --- | --- |
| `graphBuildMode(Mode)` | `standard_af` | `standard_af` |
| `argumentLabellingMode(Mode)` | `grounded` | see [argument semantics](#argument-semantics) |
| `statementLabellingMode(Mode)` | `statement` | `statement`, `statement_pass_through`, `statement_binary` |
| `graphExtension(Mode)` | `standardPref` | `standardPref`, `defeasiblePref`, `defeasibleAllPref`, `rebutRestriction`, `bp` |
| `orderingPrinciple(Mode)` | `last` | `last`, `weakest` |
| `orderingComparator(Mode)` | `elitist` | `elitist`, `democrat`, `normal` |
| `queryMode` | enabled | the fact is present or absent |
| `autoTransposition` | disabled | the fact is present or absent |
| `modulesPath(Path)` | `none` | a directory path or a base URL |

> [!NOTE]
> `graphBuildMode`, `argumentLabellingMode`, `statementLabellingMode`, `orderingPrinciple` and
> `orderingComparator` must always have a value. `FlagsBuilder` fills them in for you, so you only need to
> write them by hand when you build a theory that supplies its own flags (see
> [empty flag sets](#empty-flag-sets)).

---

## Graph construction

### `graphBuildMode(Mode)`

Selects how the argumentation graph is built from the parsed theory. `standard_af` is the only mode currently
implemented; it builds arguments, attacks and supports according to ASPIC<sup>+</sup>.

### `graphExtension(Mode)`

Post-processing steps applied to the graph once it has been built. Unlike the other flags, `graphExtension`
**may appear several times**: every occurrence adds an extension, and all of them are applied.

| Value | Effect |
| --- | --- |
| `standardPref` | ASPIC<sup>+</sup> static preferences. Attacks that do not succeed as defeats are removed according to the argument ordering. |
| `defeasiblePref` | Dung's model for defeasible preferences, where preferences are themselves defeasible conclusions. |
| `defeasibleAllPref` | The enhanced defeasible model, enabling all ASPIC<sup>+</sup> comparators. |
| `rebutRestriction` | Applies the *restricted rebut* constraint: an argument cannot rebut a conclusion drawn by a strict rule. |
| `bp` | Meta-evaluation for burden of persuasion. With this extension the `bp` indications are written inside rules, e.g. `r : [] => bp(something).` |

The defeasible preference models and the meta-argumentation treatment of the burden of persuasion are
described in the papers listed under [References]({{% ref "/docs/references" %}}).

To disable preference handling altogether, pass an empty list of extensions
(`FlagsBuilder(graphExtensions = emptyList())`) rather than omitting the flag.

> [!WARNING]
> In the IDE the _Unrestricted Rebut_ checkbox is the **inverse** of this flag: leaving it unticked adds
> `graphExtension(rebutRestriction)`. The _Meta Bp_ checkbox corresponds to `graphExtension(bp)`.

---

## Argument semantics

`argumentLabellingMode(Mode)` selects the semantics used to label arguments as **IN**, **OUT** or **UND**.

| Value | Semantics |
| --- | --- |
| `grounded` | Grounded semantics. The default, and the only one that always yields exactly one labelling. |
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
> **One solution per labelling.** Semantics such as `preferred`, `stable`, `ideal`, `cf2` and `stage2` admit
> several labellings. The engine returns them as **distinct solutions**: iterate over the solution sequence to
> see them all, instead of taking only the first one.

---

## Statement semantics

`statementLabellingMode(Mode)` decides how argument labels are propagated to the statements they conclude.

| Value | Effect |
| --- | --- |
| `statement` | Standard labelling: a statement is IN if some IN argument concludes it, OUT if all arguments concluding it are OUT, UND otherwise. |
| `statement_pass_through` | Propagates argument labels directly, without re-deriving the statement status. |
| `statement_binary` | Two-valued labelling: everything that is not IN is reported as OUT, merging OUT and UND. |

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

- `elitist` — an argument is weaker if *some* of its rules is weaker than some rule of the other;
- `democrat` — an argument is weaker if *all* its rules are weaker;
- `normal` — the plain set comparison, without elitist or democrat relaxation.

---

## Evaluation behaviour

### `queryMode`

Enables the goal-directed (structured) evaluation used by [`answerQuery/4`]({{% ref "/docs/predicate" %}}).
Instead of building the whole graph, the engine builds only the sub-graph relevant to the query, which is
considerably faster on large theories. It is **enabled by default** by `FlagsBuilder`.

### `autoTransposition`

Closes the theory under transposition: for every strict rule the contrapositive variants are added
automatically. This is required by some rationality postulates, and is **disabled by default** because it
enlarges the theory.

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

## Flags with no Kotlin equivalent

A few flags are read only from the theory text; `FlagsBuilder` has no field for them. Write the fact directly
in your theory:

| Flag | Meaning |
| --- | --- |
| `naturalTerms` | Enables normalisation of natural-language-like terms while parsing rules. |
| `metaConflicts` | Enables conflict detection between meta-level statements. |
| `metaRules` | Enables meta-level rules in the translation phase. |

---

## Empty flag sets

`FlagsBuilder().empty(true)` disables the injection of *every* flag fact. The theory then has to declare all
the required flags itself:

```kotlin
val solver = Arg2pSolverFactory.default(
    theory = """
        graphBuildMode(standard_af).
        argumentLabellingMode(grounded).
        statementLabellingMode(statement).
        orderingPrinciple(last).
        orderingComparator(elitist).
        graphExtension(standardPref).
        queryMode.

        f1 :=> d.
        f2 :=> -d.
    """.trimIndent(),
    settings = FlagsBuilder().empty(true).create(),
)
```

This is the form used by the [JavaScript bridge]({{% ref "/docs/run" %}}), which receives its flags as Prolog
text rather than as a `FlagsBuilder`.
